import Piece.Category
import Piece.Category.PieceCategory

object ChessProblem {

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

  private def place_piece(fields: Seq[(Int, Int)], state: PieceState, newPieceCat: PieceCategory): Int = {
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

  private def getPiece(cat: PieceCategory, coords: (Int, Int)): Piece = {
    cat match {
      case Category.KING => King(coords)
      case Category.QUEEN => Queen(coords)
      case Category.BISHOP => Bishop(coords)
      case Category.ROOK => Rook(coords)
      case Category.KNIGHT => Knight(coords)
    }
  }

  // assuming there's available fields & pieces
  private def backtrack_step(fields: Seq[(Int, Int)], state: PieceState): Int = {
    lazy val hlp: (PieceCategory => Int) = (cat: PieceCategory) => place_piece(fields, state removePieceFromLeft cat, cat)
    if (state.piecesLeft(Category.QUEEN) != 0)
      hlp(Category.QUEEN)
    else if (state.piecesLeft(Category.ROOK) != 0)
      hlp(Category.ROOK)
    else if (state.piecesLeft(Category.BISHOP) != 0)
      hlp(Category.BISHOP)
    else if (state.piecesLeft(Category.KING) != 0)
      hlp(Category.KING)
    else if (state.piecesLeft(Category.KNIGHT) != 0)
      hlp(Category.KNIGHT)
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

  class PieceState(val piecesLeft: Map[Piece.Category.PieceCategory, Int], val piecesOnTheBoard: List[Piece] = List(), count: Int) {
    def anyPiecesLeft(): Boolean =
      count > 0

    def this(kings: Int, queens: Int, bishops: Int, rooks: Int, knights: Int) =
      this(Map(Category.KING -> kings, Category.QUEEN -> queens, Category.BISHOP -> bishops,
        Category.ROOK -> rooks, Category.KNIGHT -> knights), List(), kings + queens + bishops + rooks + knights)

    def addPieceToTheBoard(piece: Piece): PieceState = new PieceState(piecesLeft, piece :: piecesOnTheBoard, count)

    def removePieceFromLeft(cat: PieceCategory) =
      new PieceState(changePiecesLeft(decreaseCountOfCategory(cat)), piecesOnTheBoard, count - 1)

    private def decreaseCountOfCategory(n: PieceCategory) = (m: Map[PieceCategory, Int]) => {
      m + (n -> (m(n) - 1))
    }

    private def changePiecesLeft(f: (Map[PieceCategory, Int] => Map[PieceCategory, Int])): Map[PieceCategory, Int] =
      f(piecesLeft)
  }

}

