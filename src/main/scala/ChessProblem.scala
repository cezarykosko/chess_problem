import Piece.Category
import Piece.Category.PieceCategory
import scala.io.StdIn.readInt

object ChessProblem {

  def main(args: Array[String]) = {
    //assuming each parameter is given in a separate line
    println("Number of columns:")
    val horizontal = readInt()
    println("\nNumber of rows:")
    val vertical = readInt()
    println("\nNumber of kings:")
    val kings = readInt()
    println("\nNumber of queens:")
    val queens = readInt()
    println("\nNumber of bishops:")
    val bishops = readInt()
    println("\nNumber of rooks:")
    val rooks = readInt()
    println("\nNumber of knights:")
    val knights = readInt()

    val result = backtrack(genChessboard(horizontal, vertical),
      new PieceState(kings, queens, bishops, rooks, knights))

    println("\n-----------------\n\nPossible placements:" + result)
  }

  //see if pieces of the same type are placed in lexicographical order on the chessboard
  //(so that we do not count permutations)
  private def arePiecesInOrder(piece1: Piece, piece2: Piece): Boolean =
    piece1.isSameCategory(piece2) && piece1.isAfter(piece2)

  private def placePiece(fields: Seq[(Int, Int)], state: PieceState, newPieceCat: PieceCategory): Int =
    fields
      .map((coords: (Int, Int)) => {
        val newPiece = Piece.Piece(newPieceCat, coords)
        if (state.piecesOnTheBoard exists ((p: Piece) => newPiece.beats(p) || arePiecesInOrder(newPiece, p)))
          0
        else
          backtrack(fields filter (!newPiece.beats(_)), state.addPieceToTheBoard(newPiece))})
      .sum

  private def stateToNextCategory(state: PieceState): Option[PieceCategory] =
    List(Category.QUEEN,
         Category.ROOK,
         Category.BISHOP,
         Category.KING,
         Category.KNIGHT)
      .find(state.piecesLeft(_) != 0)

  // assuming there's available fields & pieces
  private def backtrackStep(fields: Seq[(Int, Int)], state: PieceState): Int =
    stateToNextCategory(state)
      .map((cat: PieceCategory) => placePiece(fields, state removePieceFromLeft cat, cat))
      .getOrElse(1)

  def genChessboard(horizontal: Int, vertical: Int): Seq[(Int, Int)] =
      for {
        i <- 1 to horizontal
        j <- 1 to vertical
      } yield (i, j)

  def backtrack(fields: Seq[(Int, Int)], state: PieceState): Int =
    if (!state.anyPiecesLeft())
      1
    else if (fields.isEmpty)
      0
    else backtrackStep(fields, state)

  class PieceState(val piecesLeft: Map[Piece.Category.PieceCategory, Int],
                   val piecesOnTheBoard: List[Piece] = List(),
                   count: Int) {
    def anyPiecesLeft(): Boolean =
      count > 0

    def this(kings: Int, queens: Int, bishops: Int, rooks: Int, knights: Int) =
      this(Map(Category.KING -> kings,
               Category.QUEEN -> queens,
               Category.BISHOP -> bishops,
               Category.ROOK -> rooks,
               Category.KNIGHT -> knights),
           List(),
           kings + queens + bishops + rooks + knights)

    def addPieceToTheBoard(piece: Piece): PieceState =
      new PieceState(piecesLeft,
                     piece :: piecesOnTheBoard,
                     count)

    def removePieceFromLeft(cat: PieceCategory) =
      new PieceState(changePiecesLeft(decreaseCountOfCategory(cat)),
                     piecesOnTheBoard,
                     count - 1)

    private def decreaseCountOfCategory(n: PieceCategory) = (m: Map[PieceCategory, Int]) => {
      m + (n -> (m(n) - 1))
    }

    private def changePiecesLeft(f: (Map[PieceCategory, Int] => Map[PieceCategory, Int])): Map[PieceCategory, Int] =
      f(piecesLeft)
  }
}
