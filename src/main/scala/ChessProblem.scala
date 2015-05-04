object ChessProblem {

  final private val KINGS = 0
  final private val QUEENS = 1
  final private val BISHOPS = 2
  final private val ROOKS = 3
  final private val KNIGHTS = 4

  def main(args: Array[String]) = {
    //assuming each parameter is given in a separate lineg
    def read() = scala.io.StdIn.readInt()
    println("\nNumber of columns:")
    val horizontal = read()
    println("\nNumber of rows:")
    val vertical = read()
    println("\nNumber of kings:")
    val kings = read()
    println("\nNumber of queens:")
    val queens = read()
    println("\nNumber of bishops:")
    val bishops = read()
    println("\nNumber of rooks:")
    val rooks = read()
    println("\nNumber of knights:")
    val knights = read()

    val result = backtrack(horizontal, vertical, kings, queens, bishops, rooks, knights)
    println(result)
  }

  def backtrack(horizontal: Int, vertical: Int, kings: Int, queens: Int, bishops: Int, rooks: Int, knights: Int): Int = {
    lazy val chessboard =
      for {
        i <- 1 to horizontal
        j <- 1 to vertical
      } yield (i, j)

    backtrack(chessboard, new PieceState(kings, queens, bishops, rooks, knights))
  }

  //see if pieces of the same type are placed in lexicographical order on the chessboard
  //(so that we do not count permutations)
  private def arePiecesInOrder(piece1: Piece, piece2: Piece): Boolean =
    piece1.isSameCategory(piece2) && piece1.isAfter(piece2)

  private def place_piece(fields: Seq[(Int, Int)], state: PieceState, newPieceCat: Int): Int = {
    (for {
      coords <- fields
    } yield {
        val newPiece = getPiece(newPieceCat, coords)
        if (state.piecesOnTheBoard exists ((p: Piece) => newPiece.beats(p) || arePiecesInOrder(newPiece, p)))
          0
        else
          backtrack(fields filter (!newPiece.beats(_)), state.addPieceToTheBoard(newPiece))
      }).sum
  }

  private def getPiece(cat: Int, coords: (Int, Int)): Piece = {
    cat match {
      case KINGS => King(coords)
      case QUEENS => Queen(coords)
      case BISHOPS => Bishop(coords)
      case ROOKS => Rook(coords)
      case KNIGHTS => Knight(coords)
    }
  }

  // assuming there's available fields & pieces
  private def backtrack_step(fields: Seq[(Int, Int)], state: PieceState): Int = {
    lazy val hlp: (Int => Int) = (cat: Int) => place_piece(fields, state removePieceFromLeft cat, cat)
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

  class PieceState(val piecesLeft: Array[Int], val piecesOnTheBoard: List[Piece] = List(), count: Int) {
    def anyPiecesLeft(): Boolean =
      count > 0

    def this(kings: Int, queens: Int, bishops: Int, rooks: Int, knights: Int) =
      this(Array(kings, queens, bishops, rooks, knights), List(), kings + queens + bishops + rooks + knights)

    def addPieceToTheBoard(piece: Piece): PieceState = new PieceState(piecesLeft, piece :: piecesOnTheBoard, count)

    def removePieceFromLeft(cat: Int) =
      new PieceState(changePiecesLeft(remove(cat)), piecesOnTheBoard, count - 1)

    private def remove(n: Int) = (m: Array[Int]) => {
      val nArr = m.clone()
      nArr(n) -= 1
      nArr
    }

    private def changePiecesLeft(f: (Array[Int] => Array[Int])): Array[Int] =
      f(piecesLeft)
  }

}

