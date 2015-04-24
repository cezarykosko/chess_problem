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

    val result = backtrack(chessboard, new PieceState(kings, queens, bishops, rooks, knights))
    println(result)
  }

  class PieceState(val piecesLeft: Array[Int], val piecesOnTheBoard: List[Piece] = List(), count: Int) {
    def anyPiecesLeft(): Boolean =
      count > 0

    def this(kings: Int, queens: Int, bishops: Int, rooks: Int, knights: Int) =
      this(Array(kings, queens, bishops, rooks, knights), List(), kings + queens + bishops + rooks + knights)

    def addPieceToTheBoard(piece: Piece): PieceState = new PieceState(piecesLeft, piece :: piecesOnTheBoard, count)

    private def remove(n: Int) = (m: Array[Int]) => {
      val nArr = m.clone()
      nArr(n) -= 1
      nArr
    }

    private def changePiecesLeft(f: (Array[Int] => Array[Int])): Array[Int] =
      f(piecesLeft)

    def removePieceFromLeft(cat: Int) =
      new PieceState(changePiecesLeft(remove(cat)), piecesOnTheBoard, count - 1)
  }

  //see if pieces of the same type are placed in lexicographical order on the chessboard
  //(so that we do not count permutations)
  private def arePiecesInOrder(piece1: Piece, piece2: Piece): Boolean =
    piece1.isSameType(piece2) && isAfter(piece1.coords, piece2.coords)

  private def isAfter(coord1: (Int, Int), coord2: (Int, Int)): Boolean =
    coord1._1 > coord2._1 || (coord1._1 == coord2._1 && coord1._2 > coord2._2)

  private def place_piece(fields: Seq[(Int, Int)], state: PieceState, newPiece: Piece): Int = {
    (for {
      coords <- fields
    } yield {
        newPiece.coords = coords
        if (state.piecesOnTheBoard exists ((p: Piece) => newPiece.beats(p) || arePiecesInOrder(newPiece, p)))
          0
        else
          backtrack(fields filter (!newPiece.beats(_)), state.addPieceToTheBoard(newPiece))
      }).sum
  }

  private def getPiece(cat: Int): Piece = {
    cat match {
      case KINGS => new King
      case QUEENS => new Queen
      case BISHOPS => new Bishop
      case ROOKS => new Rook
      case KNIGHTS => new Knight
    }
  }

  // assuming there's available fields & pieces
  private def backtrack_step(fields: Seq[(Int, Int)], state: PieceState): Int = {
    lazy val hlp: (Int => Int) = (cat: Int) => place_piece(fields, state removePieceFromLeft cat, getPiece(cat))
    if (state.piecesLeft(QUEENS) != 0)
      hlp(QUEENS)
    else if (state.piecesLeft(ROOKS) != 0)
      hlp(ROOKS)
    else if (state.piecesLeft(BISHOPS) != 0)
      hlp(BISHOPS)
    else if (state.piecesLeft(KINGS) != 0)
      hlp(KINGS)
    else if (state.piecesLeft(KNIGHTS) != 0)
      hlp(KNIGHTS)
    else
      1
  }

  private def backtrack(fields: Seq[(Int, Int)], state: PieceState): Int = {
    if (!state.anyPiecesLeft())
      1
    else if (fields.isEmpty)
      0
    else backtrack_step(fields, state)
  }
}

