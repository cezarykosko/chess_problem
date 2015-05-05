// NOTE: assuming the coords are not fixed, all the classes will have these set to a default value
// and will have to be explicitly placed on the board, in particular

object PieceCategory extends Enumeration {
  type PieceCategory = Value
  val King, Queen, Bishop, Rook, Knight = Value
}

import PieceCategory._

sealed abstract class Piece(protected val coords: (Int, Int), private val category: PieceCategory) {

  // NOTE: assuming a piece beats its own coords
  def beats(coords: (Int, Int)): Boolean

  def beats(piece: Piece): Boolean =
    this.beats(piece.coords)

  def isSameCategory(piece: Piece): Boolean = this.category == piece.category

  private final val abs = Math.abs(_: Int)

  protected val distHorizontal = (c1: (Int, Int), c2: (Int, Int)) => abs(c1._1 - c2._1)

  protected val distVertical = (c1: (Int, Int), c2: (Int, Int)) => abs(c1._2 - c2._2)

  protected val straightBeats = (coords: (Int, Int)) =>
    distHorizontal(this.coords, coords) == 0 || distVertical(this.coords, coords) == 0

  protected val diagonalBeats = (coords: (Int, Int)) =>
    distHorizontal(this.coords, coords) == distVertical(this.coords, coords)

  private val isAfter = (coord1: (Int, Int), coord2: (Int, Int)) =>
    coord1._1 > coord2._1 || (coord1._1 == coord2._1 && coord1._2 > coord2._2)

  def isAfter(piece: Piece): Boolean =
    isAfter(this.coords, piece.coords)
}

case class King(override val coords: (Int, Int)) extends Piece(coords, PieceCategory.King) {
  override def beats(coords: (Int, Int)): Boolean =
    Math.max(this.distHorizontal(this.coords, coords), this.distVertical(this.coords, coords)) <= 1
}

case class Queen(override val coords: (Int, Int)) extends Piece(coords, PieceCategory.Queen) {
  override def beats(coords: (Int, Int)): Boolean =
    this.diagonalBeats(coords) || this.straightBeats(coords)
}

case class Bishop(override val coords: (Int, Int)) extends Piece(coords, PieceCategory.Bishop) {
  override def beats(coords: (Int, Int)) = diagonalBeats(coords)
}

case class Rook(override val coords: (Int, Int)) extends Piece(coords, PieceCategory.Rook) {
  override def beats(coords: (Int, Int)) = straightBeats(coords)
}

case class Knight(override val coords: (Int, Int)) extends Piece(coords, PieceCategory.Knight) {
  override def beats(coords: (Int, Int)) = {
    val dists = (this.distHorizontal(this.coords, coords), this.distVertical(this.coords, coords))
      dists ==(0, 0) || dists ==(2, 1) || dists ==(1, 2)
  }
}
