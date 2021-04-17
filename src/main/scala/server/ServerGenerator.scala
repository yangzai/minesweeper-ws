package server

import cats.implicits._
import eu.timepit.refined.auto._
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder
import server.types._
import shared._
import sttp.capabilities.fs2.Fs2Streams
import sttp.tapir.CodecFormat
import sttp.tapir.server.http4s.Http4sServerInterpreter
import sttp.tapir.ztapir._
import zio._
import zio.clock.Clock
import zio.console.Console
import zio.interop.catz._
import zio.logging._
import zio.random._
import zio.stream.ZStream
import zio.stream.interop.fs2z._


object ServerGenerator extends zio.App {
  def shuffle[A, Collection[+Element] <: Iterable[Element]]
  (collection: Collection[A])(implicit bf: BuildFrom[Collection[A], A, Collection[A]]): URIO[Random, Collection[A]] =
    ZIO.accessM(_.get.shuffle(collection))

  def gridRefsStr(gridRefs: Vector[Vector[Ref[SCell]]]): UIO[String] =
    ZIO.foreachPar(gridRefs) { row =>
      ZIO.foreachPar(row)(_.get.map {
        case SCell.ValueOpened(v) => (v + 48).toChar
        case SCell.BombOpened => '*'
        case _ => '□'
      }).map(_.mkString)
    }.map(_.mkString("\n"))

  def gridRefsAns(gridRefs: Vector[Vector[Ref[SCell]]]): UIO[String] =
    ZIO.foreachPar(gridRefs) { row =>
      ZIO.foreachPar(row)(_.get.map {
        case SCell.ValueOpened(v) => (v + 48).toChar
        case SCell.ValueClosed(v) => (v + 48).toChar
        case SCell.BombOpened | SCell.BombClosed => '*'
        case _ => '□'
      }).map(_.mkString)
    }.map(_.mkString("\n"))

  //ZIO is trampolined so this is stack-safe
  def processOpen(lvl: Level, sr: Ref[State], cr: Ref[Int], gr: Vector[Vector[Ref[SCell]]], x: Int, y: Int): URIO[Random with Logging, String] =
    for {
      st <- sr.get
      res <- (st, gr.size) match {
        case (State.Play, 0)  => ZIO.succeed("open: Not started")
        case (State.Play, _) => safeIndex(gr, x, y).fold("open: Out of bounds".pure[URIO[Random with Logging, *]]) { r =>
          for {
            maybeOpenStatus <- r.modifySome(none[OpenAttempt]) {
              case SCell.ValueClosed(v) => (OpenAttempt.Success(v).some, SCell.ValueOpened(v))
              case SCell.BombClosed | SCell.BombOpened => (OpenAttempt.Failure.some, SCell.BombOpened)
            }
            s <- maybeOpenStatus.fold("open: OK".pure[URIO[Random with Logging, *]]) {
              case OpenAttempt.Success(v) =>
                for {
                  _ <- ZIO.foreachPar_(getNeighbourIndices(gr, x, y)) { case (i, j) =>
                    processOpen(lvl, sr, cr, gr, i, j)
                  }.when(v == 0)
                  i <- cr.updateAndGet(_ - 1)
                  s <- if (i == 0) sr.set(State.Win(lvl.password)).as(s"open: You win. The password for this level is: ${lvl.password}")
                  else "open: OK".pure[UIO]
                } yield s
              case OpenAttempt.Failure =>
                sr.set(State.Lose).as("open: You lose")
            }
          } yield s
        }
        case (State.Win(p), _) => s"open: You win. The password for this level is: $p".pure[UIO]
        case (State.Lose, _) => "open: You lose".pure[UIO]
      }
    } yield res

  def processRequest(lr: Ref[Level], sr: Ref[State], cr: Ref[Int], grr: Ref[Vector[Vector[Ref[SCell]]]], req: Req): URIO[Random with Logging, String] = req match {
    case Req.New(lvl) =>
      val GameProperties(x, y, b) = lvl.gameProperties
      val notB = x * y - b
      for {
        v <- shuffle(Vector.fill(b)(SCell.BombClosed) ++ Vector.fill(notB)(SCell.ValueClosed(0)))
        vr <- ZIO.foreachPar(v.grouped(y).toVector)(ZIO.foreachPar(_)(Ref.make))
        _ <- ZIO.foreachPar_(vr.zipWithIndex) { case (r, i) => ZIO.foreachPar_(r.zipWithIndex) { case (ref, j) =>
          ZIO.foreachPar_(getNeighbourIndices(vr, i, j)) { case (ii, jj) =>
            vr(ii)(jj).updateSome { case cc @ SCell.ValueClosed(_) => cc.copy(v = cc.v + 1) }
          }.whenM(ref.get.map(_ == SCell.BombClosed))
        }}
        _ <- lr.set(lvl) &> cr.set(notB) &> sr.set(State.Play) &> grr.set(vr)
      } yield "new: OK"
    case Req.Open(y, x) => (lr.get <&> grr.get).flatMap { case (lvl, gr) => processOpen(lvl, sr, cr, gr, x, y) }
    case Req.Help => """help      - returns valid commands
                       |new L     - starts new session, L=1|2|3|4
                       |map       - returns the current map
                       |open X Y  - opens cell at X,Y coordinates""".stripMargin.pure[UIO]
    case Req.Map =>
      for {
        g <- grr.get
        s <- if (g.nonEmpty) gridRefsStr(g).map(m => s"map:\n$m") else "map: Not started".pure[UIO]
      } yield s
    case u @ Req.Unknown(_) => log.info(u.toString).as("Unknown command. Send 'help' to view documentation.")
  }

  val pipe: fs2.Pipe[ZIO[Random with Console with Clock, Throwable, *], Req, String] = { s =>
    (for {
      ((((sr, grr), lr), cr), sId) <- ZStream.fromEffect(
        Ref.make[State](State.Play) <&> Ref.make(Vector.empty[Vector[Ref[SCell]]]) <&>
          Ref.make(Level(1)) <&> Ref.make(0) <&> nextLong
      )
      req <- s.toZStream()
      res <- ZStream.fromEffect(processRequest(lr, sr, cr, grr, req))
        .provideSomeLayer[Random with Console with Clock](Logging.console(LogLevel.Info) >>> Logging.withRootLoggerName(s"session-$sId"))
    } yield res).toFs2Stream
  }

  val ep = endpoint.get.in("minesweeper")
    .out(webSocketBody[Req, CodecFormat.TextPlain, String, CodecFormat.TextPlain](Fs2Streams[RIO[Random with Console with Clock, *]]))

  val route = Http4sServerInterpreter.toRoutes(ep)(_ => ZIO.right(pipe))

  val serve = ZIO.runtime[ZEnv].flatMap { implicit runtime =>
    BlazeServerBuilder[RIO[Random with Console with Clock, *]](runtime.platform.executor.asEC)
      .bindHttp(8080, "localhost")
      .withHttpApp(Router("/" -> route).orNotFound)
      .serve
      .compile
      .drain
  }

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = serve.exitCode
}
