object ChessProblem {

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

    val result = backtrack(chessboard, Array(kings, queens, bishops, rooks, knights), count, List())//new PieceState(kings, queens, bishops, rooks, knights))
    println(result)
  }

  //TODO: rename
  private def isAfter(piece1: Piece, piece2: Piece): Boolean =
    piece1.getClass == piece2.getClass && isAfter(piece1.coords, piece2.coords)

  private def isAfter(coord1: (Int, Int), coord2: (Int, Int)): Boolean =
    coord1._1 > coord2._1 || (coord1._1 == coord2._1 && coord1._2 > coord2._2)

  private def remove(n: Int) = (m: Array[Int]) => {
    val nArr = m.clone()
    nArr(n) -= 1
    nArr
  }

  private def place_piece(fields: Seq[(Int, Int)], piecesLeft: Array[Int], sparePiecesCount: Int, piecesOnTheBoard: List[Piece], newPiece: Piece): Int = {
    (for {
      coords <- fields
    } yield {
        newPiece.coords = coords
        if (piecesOnTheBoard exists ((p: Piece) => newPiece.beats(p) || isAfter(newPiece, p)))
          0
        else
          backtrack(fields filter (!newPiece.beats(_)), piecesLeft, sparePiecesCount, newPiece :: piecesOnTheBoard)
      }).sum
  }

  // assuming there's available fields & pieces
  private def backtrack_step(fields: Seq[(Int, Int)], piecesLeft: Array[Int], sparePiecesCount: Int, piecesOnTheBoard: List[Piece]): Int = {
    if (piecesLeft(QUEENS) != 0)
      place_piece(fields, remove(QUEENS)(piecesLeft), sparePiecesCount - 1, piecesOnTheBoard, new Queen)
    else if (piecesLeft(ROOKS) != 0)
      place_piece(fields, remove(ROOKS)(piecesLeft), sparePiecesCount - 1, piecesOnTheBoard, new Rook)
    else if (piecesLeft(BISHOPS) != 0)
      place_piece(fields, remove(BISHOPS)(piecesLeft), sparePiecesCount - 1, piecesOnTheBoard, new Bishop)
    else if (piecesLeft(KINGS) != 0)
      place_piece(fields, remove(KINGS)(piecesLeft), sparePiecesCount - 1, piecesOnTheBoard, new King)
    else if (piecesLeft(KNIGHTS) != 0)
      place_piece(fields, remove(KNIGHTS)(piecesLeft), sparePiecesCount - 1, piecesOnTheBoard, new Knight)
    else
      1
  }

  private def backtrack(fields: Seq[(Int, Int)], piecesLeft: Array[Int], sparePiecesCount: Int, piecesOnTheBoard: List[Piece]): Int = {
    //println(fields)
    if (sparePiecesCount == 0)
      1
    else if (fields.isEmpty)
      0
    else backtrack_step(fields, piecesLeft, sparePiecesCount, piecesOnTheBoard)
  }
}

