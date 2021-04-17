package client
import zio._

package object types {
  final case class WSBuffer(sendQ: Queue[String], receiveQ: Queue[String]) {
    def send(s: String): UIO[Boolean] = sendQ.offer(s)
    val receive: UIO[String] = receiveQ.take
    def receiveN(n: Int): UIO[List[String]] = receiveQ.takeN(n)
  }

  sealed trait Cell extends Product with Serializable
  object Cell {
    case object Closed extends Cell
    case object Flagged extends Cell
    case object Opening extends Cell
  final case class Opened(value: Int) extends Cell
  }

  final case class PartitionIdx(os: Set[(Int, Int)], xs: Set[(Int, Int)], bs: Set[(Int, Int)])
}
