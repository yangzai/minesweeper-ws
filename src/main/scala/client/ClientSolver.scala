package client

import client.types._
import shared._
import sttp.client3._
import sttp.client3.httpclient.zio._
import zio._
import zio.clock._
import zio.console.Console
import zio.duration.Duration
import zio.logging._

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ParSet

object ClientSolver extends zio.App {
  type Grid = Vector[Vector[Cell]]
  type GridRefs = Vector[Vector[Ref[Cell]]]

  val zioPlayState = ZIO.succeed(State.Play)

  def gridRefsStr(gridRefs: GridRefs): UIO[String] =
    ZIO.foreachPar(gridRefs) { row =>
      ZIO.foreachPar(row)(_.get.map {
        case Cell.Opened(v) => (v + 48).toChar
        case Cell.Opening => '?'
        case Cell.Closed => '□'
        case Cell.Flagged => '*'
      }).map(_.mkString)
    }.map(_.mkString("\n"))

  def parseGrid(s: String): Grid =
    s.split('\n').toVector.drop(1).map(_.toVector.map {
      case c if c.isDigit => Cell.Opened(c.asDigit)
      case _ => Cell.Closed
    })

  def makeGridRefs(g: Grid): UIO[GridRefs] = ZIO.foreachPar(g)(ZIO.foreachPar(_)(Ref.make))

  def clusterIndices(set: ParSet[(Int, Int)], deg: Int): ParSet[Set[(Int, Int)]] = {
    val empty = ParSet.empty[Set[(Int, Int)]]

    def f(acc: ParSet[Set[(Int, Int)]], s: Set[(Int, Int)]) = {
      val (clusterSets, others) = acc.partition(_.exists { case (i, j) => s.exists { case (ii, jj) => Math.abs(ii - i) <= deg && Math.abs(jj - j) <= deg } })

      others + clusterSets.fold(s)(_ ++ _)
    }

    set.map(Set(_)).aggregate(empty)(f, (s1, s2) => (s1 ++ s2).foldLeft(empty)(f))
  }

  @tailrec def reduceConstraints(set: ParSet[(Int, Set[(Int, Int)])]): ParSet[(Int, Set[(Int, Int)])] = {
    //flag detects if a reduction has happened
    val (flag, ss) = set.foldLeft((false, ParSet.empty[(Int, Set[(Int, Int)])])) { case ((f, acc), t@(b, s)) =>
      //subsets reduces current set, current set reduces supersets
      val (subsets, nonSubsets) = acc.partition { case (_b, _s) => _b <= b && _s.subsetOf(s) }

      val reducedT@(reducedB, reducedS) = subsets.foldLeft(t) { case ((bb, ss), (_b, _s)) => (bb - _b, ss -- _s) }
      if (reducedS.nonEmpty) {
        val (supersets, others) = nonSubsets.partition { case (_b, _s) => _b >= reducedB && reducedS.subsetOf(_s) }

        (subsets.nonEmpty || supersets.nonEmpty || f,
          (subsets ++ others + reducedT ++ supersets.map { case (_b, _s) => (_b - reducedB, _s -- reducedS) }).filter(_._2.nonEmpty)
        )
      } else (true, acc)
    }

    //    assert(flag == (ss != set))

    //repeat until no more reductions
    if (flag) reduceConstraints(ss) else ss
  }

  def wsReq(lvl: Int) = basicRequest.get(uri"ws://localhost:8080/minesweeper") response asWebSocketAlways[RIO[Clock with Logging, *], State] { ws =>
    def play(gridRefs: GridRefs, activesRef: Ref[Set[(Int, Int)]], buff: WSBuffer): RIO[Logging, State] = {
      def getNeighbourIndices(x: Int, y: Int, deg: Int = 1): Set[(Int, Int)] =
        shared.getNeighbourIndices(gridRefs, x, y, deg)

      def partitionIndices(s: Set[(Int, Int)]): UIO[PartitionIdx] =
        ZIO.foldLeft(s)(PartitionIdx(Set.empty, Set.empty, Set.empty)) { case (p@PartitionIdx(os, xs, bs), i@(x, y)) =>
          gridRefs(x)(y).get.map {
            case Cell.Opened(_) | Cell.Opening => p.copy(os = os + i)
            case Cell.Closed => p.copy(xs = xs + i)
            case Cell.Flagged => p.copy(bs = bs + i)
          }
        }

      //simple candidate select for guessing
      // max(closed neighbour size / open neighbour size)
      def selectCandidate(xs: Vector[(Int, Int)]): Task[(Int, Int)] =
        for {
          withSortKeys <- ZIO.foreachPar(xs) { case t@(i, j) =>
            partitionIndices(getNeighbourIndices(i, j)).map {
              case PartitionIdx(os, xs, _) if os.nonEmpty => t -> xs.size.toDouble / os.size
              case _ => t -> Double.PositiveInfinity
            }
          }
          (x, _) <- ZIO.fromOption(withSortKeys.maxByOption(_._2))
            .orElseFail(new Error("No valid candidate. Invalid state - game should be over."))
        } yield x

      def openResHandler(s: String): URIO[Logging, State] = s match {
        case s"open: OK" => zioPlayState
        case s"open: You win. The password for this level is: $s" => ZIO.succeed(State.Win(s))
        case s"open: You lose" => ZIO.succeed(State.Lose)
        case _ => log.warn(s"received unexpected msg: $s") *> zioPlayState
      }

      def mapResHandler(s: String): URIO[Logging, Unit] = s match {
        case s if s startsWith "map:" =>
          ZIO.foreachPar_(gridRefs.zip(parseGrid(s)).zipWithIndex) { case ((rs, cs), i) =>
            ZIO.foreachPar_(rs.zip(cs).zipWithIndex) { case ((r, c), j) =>
              ZIO.whenCase(c) { case Cell.Opened(v) =>
                //try update opened cell, but only add to active indices if opened cell > 0 && cell is actually updated
                activesRef.update(_ + ((i, j))).when(v > 0)
                  .whenM(r.modifySome(false) { case Cell.Closed | Cell.Opening => (true, c) })
              }
            }
          }
        case _ => log.warn(s"received unexpected msg: $s")
      }

      //remove from active indices
      def complete(i: Int, j: Int): UIO[Unit] = activesRef.update(_ - ((i, j)))

      def getCellIfIsActive(i: Int, j: Int): IO[Option[Nothing], Cell] =
        ZIO.ifM(activesRef.map(_.contains(i, j)).get)(gridRefs(i)(j).get, ZIO.fail(None))

      def openCellWith[R, E, A](x: Int, y: Int, onModifiedSuccess: ZIO[R, E, A], otherwise: ZIO[R, E, A]): ZIO[R, E, A] =
        ZIO.ifM(gridRefs(x)(y).modifySome(false) { case Cell.Closed => (true, Cell.Opening) })(onModifiedSuccess, otherwise)

      (for {
        //counter for open request
        count <- Ref.make(0)

        // `outerFlag` indicates grid changes (open/mark) in a all iterations
        outerFlag <- Ref.make(false)

        //actives cells are opened cells that still have closed neighbours
        actives <- activesRef.get

        (states, _) <- (for {
          // `innerFlag` indicates grid changes (open/mark) in a single iteration
          innerFlag <- Ref.make(false)

          //helper functions to perform all effects required for opening and marking cells
          //note1: received msgs might not match with the same open call
          // but it's fine as long as we end up receiving the same number of msgs
          //note2:  grid changes from open is already tracked by count, it will be propagated to innerFlag at a later stage
          open = (x: Int, y: Int) => openCellWith(x, y, buff.send(s"open $y $x") *> count.update(_ + 1), ZIO.unit)
          mark = (x: Int, y: Int) => innerFlag.set(true)
            .whenM(gridRefs(x)(y).modifySome(false) { case Cell.Closed => (true, Cell.Flagged) })

          reducedRef <- Ref.make(Set.empty[(Int, Set[(Int, Int)])])

          //foreach active cell, apply constraint rules to deduce cells for opening or marking
          _ <- ZIO.foreachPar_(actives) { case (i, j) =>
            val neis = getNeighbourIndices(i, j)
            gridRefs(i)(j).get.flatMap {
              case Cell.Opened(v) if v > 0 =>
                for {
                  //rule 1: foreach 2nd degree neighbour that is also opened
                  // determine difference in closed 1st degree neighbours and difference remaining bombs
                  active2DegNeis <- ZIO.collectAllSuccessesPar(getNeighbourIndices(i, j, 2).collect {
                    case (ii, jj) if ii > i || (ii == i && jj > j) =>
                      //filters half of the possible neighbours to prevent double run
                      //i.e we only need those in square brackets `[]`
                      //(0,0)(0,1)(0,2)(0,3)(0,4)
                      //(1,0)(1,1)(1,2)(1,3)(1,4)
                      //(2,0)(2,1){i,j}[2,3][2,4]
                      //[3,0][3,1][3,2][3,3][3,4]
                      //[4,0][4,1][4,2][4,3][4,4]
                      //It doesn't have to be same half as the example above
                      //As long as the selected half is rotationally symmetrical with the unselect half it should work
                      getCellIfIsActive(ii, jj).map((ii, jj, _))
                  }.toVector)
                  _ <- ZIO.foreachPar_(active2DegNeis) {
                    case (ii, jj, Cell.Opened(vv)) if vv > 0 =>
                      val _neis = getNeighbourIndices(ii, jj)
                      for {
                        PartitionIdx(_, _xs, _bs) <- partitionIndices(_neis ++ neis)
                        xs1 = _xs & neis
                        xs2 = _xs & _neis
                        bs1 = _bs & neis
                        bs2 = _bs & _neis

                        //diff sets
                        backSet = xs1 -- xs2
                        fwdSet = xs2 -- xs1

                        //bomb left difference
                        bLeftDelta = (vv - bs2.size) - (v - bs1.size)

                        (openSet, markSet) = (bLeftDelta, fwdSet, backSet) match {
                          //if there is no change in bomb left and one of the diff set is empty
                          // open all in the other diff set
                          case (0, fSet, bSet) if fSet.isEmpty => (bSet, Set.empty[(Int, Int)])
                          case (0, fSet, bSet) if bSet.isEmpty => (fSet, Set.empty[(Int, Int)])

                          //if the change in bomb left == size of diff set in the same direction
                          // mark all in that diff set and open all in the other diff set
                          case (bLD, fSet, bSet) if bLD > 0 && bLD == fSet.size => (bSet, fSet)
                          case (bLD, fSet, bSet) if bLD < 0 && -bLD == bSet.size => (fSet, bSet)

                          //otherwise do nothing
                          case _ => (Set.empty[(Int, Int)], Set.empty[(Int, Int)])
                        }

                        //perform marking and opening
                        _ <- ZIO.foreachPar_(markSet) { case (x, y) => mark(x, y) } &> ZIO.foreachPar_(openSet) { case (x, y) => open(x, y) }
                      } yield ()
                    case _ => ZIO.unit
                  }

                  //rule 2 part 1: foreach 2nd degree neighbour that is also opened
                  // reduce constraints if 1st degree neighbours are subsets
                  _ <- for {
                    (b, rest) <- ZIO.foldLeft(getNeighbourIndices(i, j, 2))(0, neis) { case (acc@(_b, _rest), (ii, jj)) =>
                      getCellIfIsActive(ii, jj).optional.flatMap {
                        case Some(Cell.Opened(vv)) if vv > 0 =>
                          partitionIndices(getNeighbourIndices(ii, jj)).map { case PartitionIdx(_, _xs, _bs) =>
                            if (_xs.subsetOf(_rest)) (_b + (vv - _bs.size), _rest -- _xs) else acc
                          }
                        case _ => ZIO.succeed(acc)
                      }
                    }
                    PartitionIdx(_, rxs, rbs) <- partitionIndices(rest)
                    _ <- (v - b - rbs.size, rxs) match {
                      //if reduced bomb == 0, open all in reduced set
                      case (0, s) => ZIO.foreachPar_(s) { case (x, y) => log.debug(s"r2p1 open $y $x") *> open(x, y) }

                      //if reduced bomb == size of reduced set, mark all in reduced set
                      case (bl, s) if bl == s.size => ZIO.foreachPar_(s) { case (x, y) => log.debug(s"r2p1 mark $y $x") *> mark(x, y) }

                      //otherwise collate reductions and perform further reductions in part 2
                      case (bl, s) => reducedRef.update(_ + ((bl, s)))
                    }
                  } yield ()

                  //rule 3 (base rule): foreach cell mark or open all closed neighbours
                  // if reminding bombs == closed neighbour size or reminding bomb == 0
                  //this rule also completes(remove from active) the cell
                  _ <- partitionIndices(neis).flatMap { case PartitionIdx(_, xs, bs) =>
                    (v - bs.size, xs.size) match {
                      case (_, 0) => complete(i, j)
                      case (0, _) => ZIO.foreachPar_(xs) { case (x, y) => open(x, y) } <* complete(i, j) //TODO: move flag set to inner
                      case (bLeft, xsLen) if bLeft == xsLen => ZIO.foreachPar_(xs) { case (x, y) => mark(x, y) } <* complete(i, j)
                      case _ => ZIO.unit
                    }
                  }
                } yield ()
              case _ => ZIO.unit
            }
          }

          //rule 2 part 2: perform further reductions with the same subset rule
          reducedSet <- reducedRef.map(s => reduceConstraints(s.par).to(Set)).get
          _ <- ZIO.foreachPar_(reducedSet) {
            case (0, s) => ZIO.foreachPar_(s) { case (ii, jj) => log.debug(s"r2p2 open $jj $ii") *> open(ii, jj) }
            case (_b, s) if s.size == _b => ZIO.foreachPar_(s)({ case (ii, jj) => log.debug(s"r2p2 mark $jj $ii") *> mark(ii, jj) })
            case _ => ZIO.unit
          }

          //get & reset counter, process game states
          c <- count.getAndSet(0)
          strs <- buff.receiveN(c)
          _states <- ZIO.foreachPar(strs)(openResHandler).map(_.toSet)

          //set innerFlag if open msgs have been sent
          // set outerFlag if innerFlag set
          _ <- innerFlag.set(true).when(c > 0)
          _ <- outerFlag.set(true).whenM(innerFlag.get)

          //repeat until an iteration made no change (open/mark) or the game ended
        } yield (_states, innerFlag)).repeatWhileM { case (ss, inF) => inF.get.map(f => f && ss.forall(_ == State.Play)) }

        //clustering algorithm for early guessing
        // linked border closed cells that are not neighbours with other non-border closed cells
        // form clusters with constraints that cannot be further reduced
        // would benefit from early failure if guessing is done early
        states1 <- for {
          _actives <- activesRef.get
          PartitionIdx(_, xs, _) <- partitionIndices(_actives.flatMap { case (i, j) => getNeighbourIndices(i, j) })
          clusters = clusterIndices(xs.par, 2).to(Set)

          //if theres only 1 main cluster skip
          // guessing on one cluster can only be delayed if there is another cluster that is actively being worked on
          _ <- ZIO.foreachPar(clusters) { cluster =>
            for {
              PartitionIdx(_os, _xs, _) <- partitionIndices(cluster.flatMap { case x@(i, j) => getNeighbourIndices(i, j, 2) + x })
              //candidate cluster: open neighbours of cluster not recently opened &
              // closed neighbours of cluster == cluster (i.e. all are border cells)
              _ <- ZIO.whenM(ZIO.foreachPar(_os) { case (i, j) => gridRefs(i)(j).get }.map(!_.contains(Cell.Opening) && cluster == _xs))(
                selectCandidate(_xs.toVector).flatMap { case (i, j) =>
                  openCellWith(i, j,
                    log.warn(s"open early guess $j $i") *> buff.send(s"open $j $i") *> count.update(_ + 1),
                    ZIO.fail(new Error(s"Candidate $i $j already opened."))
                  )
                }
              )
            } yield ()
          }.when(clusters.size > 1)

          //get counter, process game states
          c <- count.get
          strs <- buff.receiveN(c)
          _states <- ZIO.foreachPar(strs)(openResHandler).map(_.toSet)

          _ <- outerFlag.set(true).when(c > 0)
        } yield states ++ _states

        //perform guessing on remaining closed cells only if grid did not change
        states2 <- ZIO.ifM(outerFlag.get.negate)(
          for {
            //filter all closed indices
            xs <- ZIO.mergeAllPar(gridRefs.zipWithIndex.par.map { case (rs, i) =>
              ZIO.collectPar(rs.zipWithIndex) { case (r, j) =>
                r.get.flatMap {
                  case Cell.Closed => ZIO.succeed(i, j)
                  case _ => ZIO.fail(None)
                }
              }
            }.seq)(Vector.empty[(Int, Int)])(_ ++ _)
            (i, j) <- selectCandidate(xs)
            s <- openCellWith(i, j,
              log.warn(s"open guess $j $i") *> buff.send(s"open $j $i") *> buff.receive.flatMap(openResHandler),
              ZIO.fail(new Error(s"Candidate $i $j already opened."))
            )
          } yield states1 + s,
          ZIO.succeed(states1)
        )

        state = states2.collectFirst { case s@(State.Win(_) | State.Lose) => s }.getOrElse(State.Play)

        //update grid if game did not end
        // repeat until game ends
        _ <- (buff.send("map") *> buff.receive.flatMap(mapResHandler)).when(state == State.Play)
      } yield state).repeatUntil(_ != State.Play)
    }

    for {
      //`HttpClientZioBackend` module requires native `HttpClient` from JDK >= 11, but native Websocket methods are not thread-safe
      //Workaround: thread-safe buffer and a dedicated fibers dispatch parallel sends and receives respectively.
      buff@WSBuffer(sendQ, receiveQ) <- ZIO.mapParN(Queue.unbounded[String], Queue.unbounded[String])(WSBuffer.apply)
      fibers <- ZIO.collectAllPar(List(sendQ.take.flatMap(ws.sendText).forever.fork, ws.receiveText().flatMap(receiveQ.offer).forever.fork))

      state <- (for {
        start <- instant
        _ <- log.info(s"playing L$lvl")

        newRes <- buff.send(s"new $lvl") *> buff.receive
        _ <- log.info(newRes)

        s <- buff.send("map") *> buff.receive
        g <- makeGridRefs(parseGrid(s))

        activesRef <- Ref.make(Set.empty[(Int, Int)])
        st <- play(g, activesRef, buff)
        _ <- st match {
          case State.Lose =>
            for {
              s <- gridRefsStr(g)
              _ <- log.warn(s"\n$s") *> log.warn("Game lost. Replaying...")
            } yield ()
          case State.Win(pwd) =>
            for {
              s <- buff.send("map") *> buff.receive
              c = parseGrid(s).foldLeft(0) { case (acc, cs) => acc + cs.count(_ == Cell.Closed) }
              s <- gridRefsStr(g)
              _ <- log.info(s"\n$s") *> log.info(s"Game Won: $pwd")
              _ <- log.info(s"Size ${g.size} ${g.head.size}, Bomb $c")

            } yield ()
          case State.Play => ZIO.fail(new Error(s"Invalid end state: $st"))
        }
        end <- instant
        _ <- log.info(s"L$lvl single game duration: ${Duration.fromInterval(start, end)}")
      } yield st).repeatWhileEquals(State.Lose)
      _ <- ZIO.foreachPar_(fibers)(_.interrupt)
    } yield state
  }

  override def run(args: List[String]): URIO[ZEnv, ExitCode] =
    (for {
      //run each level with dedicated websocket connections in parallel
      states <- ZIO.foreachPar(1 to 4) { lvl =>
        val innerLogLayer = Logging.console(LogLevel.Info) >>> Logging.withRootLoggerName(s"L$lvl")
        for {
          start <- instant
          res <- sendR(wsReq(lvl)).provideSomeLayer[Console with Clock with SttpClient](innerLogLayer)
          end <- instant
          _ <- log.info(s"L$lvl full duration: ${Duration.fromInterval(start, end)}")
        } yield res.body
      }
      _ <- log.info("") *> log.info("Games completed. Game states for all levels:") *> ZIO.foreach_(states)(s => log.info(s.toString))
    } yield ()).provideCustomLayer(Logging.console() ++ HttpClientZioBackend.layer()).exitCode
}
