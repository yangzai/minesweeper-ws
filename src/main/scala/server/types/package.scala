package server

import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric.Interval
import eu.timepit.refined.refineV
import eu.timepit.refined.types.numeric.PosInt
import sttp.tapir.Codec.{PlainCodec, idPlain}

package object types {
  type _1to4 = Interval.Closed[1, 4]
  type Int1to4 = Int Refined _1to4

  sealed trait OpenAttempt
  object OpenAttempt {
    final case class Success(v: Int) extends OpenAttempt
    case object Failure extends OpenAttempt
  }
  sealed trait SCellStatus
  object SCellStatus {
    case object Opened extends SCellStatus
    case object Closed extends SCellStatus
  }
  sealed trait SCell extends Product with Serializable
  object SCell {
    final case class ValueOpened(v: Int) extends SCell
    final case class ValueClosed(v: Int) extends SCell
    case object BombOpened extends SCell
    case object BombClosed extends SCell
  }

  final case class GameProperties(x: PosInt, y: PosInt, bombs: PosInt)

  case class Level(v: Int1to4) {
    val gameProperties: GameProperties = v match {
      case Refined(1) => GameProperties(10, 10, 15)
      case Refined(2) => GameProperties(20, 40, 150)
      case Refined(3) => GameProperties(50, 100, 1000)
      case Refined(_) => GameProperties(50, 500, 3500)
    }
    val password: String = v match {
      case Refined(1) => "pwd1"
      case Refined(2) => "pwd2"
      case Refined(3) => "pwd3"
      case Refined(_) => "pwd4"
    }
  }

  sealed trait Req extends Product with Serializable
  object Req {
    final case class New(lvl: Level) extends Req
    final case class Open(y: Int, x: Int) extends Req
    case object Map extends Req
    case object Help extends Req
    final case class Unknown(s: String) extends Req

    implicit val c: PlainCodec[Req] = idPlain[String]().map {
      case s @ s"new $lvl" => lvl.toIntOption.flatMap(i => refineV[_1to4](i).map(ii => New(Level(ii))).toOption).getOrElse(Unknown(s))
      case s @ s"open $y $x" => (y.toIntOption zip x.toIntOption).map { case (yy, xx) => Open(yy, xx) }.getOrElse(Unknown(s))
      case "map" => Map
      case "help" => Help
      case s => Unknown(s)
    } {
      case New(lvl) => s"new ${lvl.v}"
      case Open(y, x) => s"open $y $x"
      case Map => "map"
      case Help => "help"
      case Unknown(s) => s
    }
  }
}

