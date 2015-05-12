// NOTE: assuming the coords are not fixed, all the classes will have these set to a default value
// and will have to be explicitly placed on the board, in particular

object Piece {

  object Category extends Enumeration {
    type PieceCategory = Value
    val KING, QUEEN, BISHOP, ROOK, KNIGHT = Value
  }

  def apply(cat: Category.PieceCategory, coords: (Int, Int)): Piece = {
    cat match {
      case Category.KING => King(coords)
      case Category.QUEEN => Queen(coords)
      case Category.BISHOP => Bishop(coords)
      case Category.ROOK => Rook(coords)
      case Category.KNIGHT => Knight(coords)
    }
  }
}

sealed abstract class Piece (protected val coords: (Int, Int),
                             protected val category: Piece.Category.PieceCategory) {

  override def toString: String = {
    category match {
      case Piece.Category.KING => "King at " + coords
      case Piece.Category.QUEEN => "Queen at " + coords
      case Piece.Category.BISHOP => "Bishop at " + coords
      case Piece.Category.ROOK => "Rook at " + coords
      case Piece.Category.KNIGHT => "Knight at " + coords
    }
  }

  // NOTE: assuming a piece beats its own coords
  def beats(coords: (Int, Int)): Boolean

  def beats(piece: Piece): Boolean =
    this.beats(piece.coords)

  def isSameCategory(piece: Piece): Boolean =
    this.category == piece.category

  protected val distHorizontal =
    (c1: (Int, Int), c2: (Int, Int)) => (c1._1 - c2._1).abs

  protected val distVertical =
    (c1: (Int, Int), c2: (Int, Int)) => (c1._2 - c2._2).abs

  protected val straightBeats =
    (coords: (Int, Int)) =>
      distHorizontal(this.coords, coords) == 0 ||
      distVertical(this.coords, coords) == 0

  protected val diagonalBeats =
    (coords: (Int, Int)) =>
      distHorizontal(this.coords, coords) == distVertical(this.coords, coords)

  private val isAfter =
    (coord1: (Int, Int), coord2: (Int, Int)) =>
      coord1._1 > coord2._1 || (coord1._1 == coord2._1 && coord1._2 > coord2._2)

  def isAfter(piece: Piece): Boolean =
    isAfter(this.coords, piece.coords)
}

case class King(override val coords: (Int, Int)) extends Piece(coords, Piece.Category.KING) {

  override def beats(coords: (Int, Int)): Boolean =
    Math.max(this.distHorizontal(this.coords, coords), this.distVertical(this.coords, coords)) <= 1
}

case class Queen(override val coords: (Int, Int)) extends Piece(coords, Piece.Category.QUEEN) {

  override def beats(coords: (Int, Int)): Boolean =
    this.diagonalBeats(coords) || this.straightBeats(coords)
}

case class Bishop(override val coords: (Int, Int)) extends Piece(coords, Piece.Category.BISHOP) {

  override def beats(coords: (Int, Int)) = diagonalBeats(coords)
}

case class Rook(override val coords: (Int, Int)) extends Piece(coords, Piece.Category.ROOK) {

  override def beats(coords: (Int, Int)) = straightBeats(coords)
}

case class Knight(override val coords: (Int, Int)) extends Piece(coords, Piece.Category.KNIGHT) {

  override def beats(coords: (Int, Int)) = {
    val dists = (this.distHorizontal(this.coords, coords),
                 this.distVertical(this.coords, coords))
    List((0, 0), (1, 2), (2, 1)) contains dists
  }
}
