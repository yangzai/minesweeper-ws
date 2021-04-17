package object shared {
  sealed trait State extends Product with Serializable
  object State {
    final case class Win(pwd: String) extends State
    case object Play extends State
    case object Lose extends State
  }

  import monocle.syntax.all._
  def safeIndex[A](g: Vector[Vector[A]], x: Int, y: Int): Option[A] = g.focus().index(x).index(y).getOption

  def getNeighbourIndices[A](gridRefs: Vector[Vector[A]], x: Int, y: Int, deg: Int = 1): Set[(Int, Int)] =
    for {
      i <- (x - deg to x + deg).toSet if i >= 0 && i < gridRefs.size
      j <- y - deg to y + deg if j >= 0 && j < gridRefs(0).size && (i, j) != (x, y)
    } yield (i, j)
}
