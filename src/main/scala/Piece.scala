// NOTE: assuming the coords are not fixed, all the classes will have these set to a default value
// and will have to be explicitly placed on the board, in particular
sealed abstract class Piece(private var _coords: Option[(Int, Int)]) {
  def coords: (Int, Int) = _coords match {
    case None => (0, 0)
    case Some(c) => c
  }

  def coords_=(c: (Int, Int)) {
    _coords = Some(c)
  }

  // NOTE: assuming a piece beats its own coords
  def beats(coords: (Int, Int)): Boolean

  def beats(piece: Piece): Boolean =
    piece._coords match {
      case None => false
      case Some(c: (Int, Int)) => this.beats(c)
    }

  def isSameType(piece: Piece): Boolean = false

  private final val abs = Math.abs(_: Int)

  protected val distHorizontal = (c1: (Int, Int), c2: (Int, Int)) => abs(c1._1 - c2._1)

  protected val distVertical = (c1: (Int, Int), c2: (Int, Int)) => abs(c1._2 - c2._2)

  protected val optionWrapper = (coords: (Int, Int), p: ((Int, Int), (Int, Int)) => Boolean) => {
    this._coords match {
      case None => false
      case Some(c: (Int, Int)) => p(coords, c)
    }
  }

  protected val straightBeats =
    optionWrapper(_: (Int, Int), (c1: (Int, Int), c2: (Int, Int)) =>
      distHorizontal(c1, c2) == 0 || distVertical(c1, c2) == 0)

  protected val diagonalBeats =
    optionWrapper(_: (Int, Int), (c1: (Int, Int), c2: (Int, Int)) =>
      distHorizontal(c1, c2) == distVertical(c1, c2))

  private val isAfter = (coord1: (Int, Int), coord2: (Int, Int)) =>
    coord1._1 > coord2._1 || (coord1._1 == coord2._1 && coord1._2 > coord2._2)

  def isAfter(piece: Piece): Boolean = {
    piece._coords match {
      case None => true
      case Some(c) => optionWrapper(c, isAfter)
    }
  }
}

case class King() extends Piece(None) {
  override def beats(coords: (Int, Int)): Boolean =
    optionWrapper(coords, (c1: (Int, Int), c2: (Int, Int)) =>
      Math.max(this.distHorizontal(c1, c2), this.distVertical(c1, c2)) <= 1)

  override def isSameType(piece: Piece): Boolean = {
    piece match {
      case King() => true
      case default => false
    }
  }
}

case class Queen() extends Piece(None) {
  override def beats(coords: (Int, Int)): Boolean =
    this.diagonalBeats(coords) || this.straightBeats(coords)

  override def isSameType(piece: Piece): Boolean = {
    piece match {
      case Queen() => true
      case default => false
    }
  }
}

case class Bishop() extends Piece(None) {
  override def beats(coords: (Int, Int)) = diagonalBeats(coords)

  override def isSameType(piece: Piece): Boolean = {
    piece match {
      case Bishop() => true
      case default => false
    }
  }
}

case class Rook() extends Piece(None) {
  override def beats(coords: (Int, Int)) = straightBeats(coords)

  override def isSameType(piece: Piece): Boolean = {
    piece match {
      case Rook() => true
      case default => false
    }
  }
}

case class Knight() extends Piece(None) {
  override def beats(coords: (Int, Int)) =
    optionWrapper(coords, (c1: (Int, Int), c2: (Int, Int)) => {
      val dists = (this.distHorizontal(c1, c2), this.distVertical(c1, c2))
      dists ==(0, 0) || dists ==(2, 1) || dists ==(1, 2)
    })

  override def isSameType(piece: Piece): Boolean = {
    piece match {
      case Knight() => true
      case default => false
    }
  }
}