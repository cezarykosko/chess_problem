sealed abstract class Piece(var coords: (Int, Int)) {

  // NOTE: assuming a pawn beats its coords
  def beats(coords: (Int, Int)): Boolean

  def beats(piece: Piece): Boolean = this.beats(piece.coords)

  private final val abs = Math.abs(_: Int)

  protected val distHorizontal = (c: (Int, Int)) => abs(this.coords._1 - c._1)

  protected val distVertical = (c: (Int, Int)) => abs(this.coords._2 - c._2)

  protected val straightBeats = (coords: (Int, Int)) =>
    distHorizontal(coords) == 0 || distVertical(coords) == 0

  protected val diagonalBeats = (coords: (Int, Int)) =>
    distHorizontal(coords) == distVertical(coords)
}

case class King extends Piece(0, 0) {
  override def beats(coords: (Int, Int)): Boolean =
    Math.max(this.distHorizontal(coords), this.distVertical(coords)) <= 1
}

case class Queen extends Piece(0, 0) {
  override def beats(coords: (Int, Int)): Boolean =
    diagonalBeats(coords) || straightBeats(coords)
}

case class Bishop extends Piece(0, 0) {
  override def beats(coords: (Int, Int)) = diagonalBeats(coords)
}

case class Rook extends Piece(0, 0) {
  override def beats(coords: (Int, Int)) = straightBeats(coords)
}

case class Knight extends Piece(0, 0) {
  override def beats(coords: (Int, Int)) = {
    val dists = (this.distHorizontal(coords), this.distVertical(coords))
    dists ==(0, 0) || dists ==(2, 1) || dists ==(1, 2)
  }
}