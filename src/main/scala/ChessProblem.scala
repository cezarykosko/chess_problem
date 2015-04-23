object ChessProblem {

  type Piece = (Int, Int, Int)

  final private val KINGS = 0
  final private val QUEENS = 1
  final private val BISHOPS = 2
  final private val ROOKS = 3
  final private val KNIGHTS = 4

  def main(args: Array[String]) = {
    def read() = Console.readInt()
    val horizontal = read()
    val vertical = read()
    val kings = read()
    val queens = read()
    val bishops = read()
    val rooks = read()
    val knights = read()

    lazy val chessboard =
      for {
        i <- 1 to horizontal
        j <- 1 to vertical
      } yield (i, j)

    val count = kings + queens + bishops + rooks + knights

    val result = backtrack(chessboard, Array(kings, queens, bishops, rooks, knights), count, List())
    println(result)
  }

  //TODO: rename
  private def isAfter(piece1: Piece, piece2: Piece): Boolean =
    piece1._3 == piece2._3 && (piece1._1 > piece2._1 || (piece1._1 == piece2._1 && piece1._2 > piece2._2))

  private final val abs = Math.abs(_: Int)

  protected val distHorizontal = (c1: (Int, Int), c2: (Int, Int)) => abs(c1._1 - c2._1)

  protected val distVertical = (c1: (Int, Int), c2: (Int, Int)) => abs(c1._2 - c2._2)

  protected val straightBeats = (c1: (Int, Int), c2: (Int, Int)) =>
    distHorizontal(c1, c2) == 0 || distVertical(c1, c2) == 0

  protected val diagonalBeats = (c1: (Int, Int), c2: (Int, Int)) =>
    distHorizontal(c1, c2) == distVertical(c1, c2)

  private def beats(piece1: Piece, piece2: Piece): Boolean =
    beats(piece1, (piece2._1, piece2._2))

  private def beats(piece: Piece, coords: (Int, Int)): Boolean = {
    val pCoords = (piece._1, piece._2)
    piece._3 match {
      case KINGS => Math.max(distHorizontal(pCoords, coords), distVertical(pCoords, coords)) <= 1
      case QUEENS => diagonalBeats(pCoords, coords) || straightBeats(pCoords, coords)
      case BISHOPS => diagonalBeats(pCoords, coords)
      case ROOKS => straightBeats(pCoords, coords)
      case KNIGHTS =>
        val dists = (distHorizontal(pCoords, coords), distVertical(pCoords, coords))
        dists ==(0, 0) || dists ==(2, 1) || dists ==(1, 2)
      case default => false
    }
  }

  private def remove(n: Int) = (m: Array[Int]) => {
    val nArr = m.clone()
    nArr(n) -= 1
    nArr
  }

  private def place_piece(fields: Seq[(Int, Int)], piecesLeft: Array[Int], sparePiecesCount: Int, piecesOnTheBoard: List[Piece], pieceType: Int): Int = {
    (for {
      coords <- fields
    } yield {
        val newPiece = (coords._1, coords._2, pieceType)

        if (piecesOnTheBoard exists ((p: Piece) => isAfter(newPiece, p) || beats(newPiece, p))) // || isAfter(newPiece, p)))
          0
        else
          backtrack(fields filter (!beats(newPiece, _)), piecesLeft, sparePiecesCount, newPiece :: piecesOnTheBoard)
      }).sum
  }

  // assuming there's available fields & pieces
  private def backtrack_step(fields: Seq[(Int, Int)], piecesLeft: Array[Int], sparePiecesCount: Int, piecesOnTheBoard: List[Piece]): Int = {
    if (piecesLeft(QUEENS) != 0)
      place_piece(fields, remove(QUEENS)(piecesLeft), sparePiecesCount - 1, piecesOnTheBoard, QUEENS)
    else if (piecesLeft(ROOKS) != 0)
      place_piece(fields, remove(ROOKS)(piecesLeft), sparePiecesCount - 1, piecesOnTheBoard, ROOKS)
    else if (piecesLeft(BISHOPS) != 0)
      place_piece(fields, remove(BISHOPS)(piecesLeft), sparePiecesCount - 1, piecesOnTheBoard, BISHOPS)
    else if (piecesLeft(KINGS) != 0)
      place_piece(fields, remove(KINGS)(piecesLeft), sparePiecesCount - 1, piecesOnTheBoard, KINGS)
    else if (piecesLeft(KNIGHTS) != 0)
      place_piece(fields, remove(KNIGHTS)(piecesLeft), sparePiecesCount - 1, piecesOnTheBoard, KNIGHTS)
    else
      1
  }

  private def backtrack(fields: Seq[(Int, Int)], piecesLeft: Array[Int], sparePiecesCount: Int, piecesOnTheBoard: List[Piece]): Int = {
    if (sparePiecesCount == 0)
      1
    //else if (fields.isEmpty)
    //  0
    else backtrack_step(fields, piecesLeft, sparePiecesCount, piecesOnTheBoard)
  }
}

