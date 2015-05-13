import java.io.{FileNotFoundException, IOException, FileWriter}

import Piece.Category
import Piece.Category.PieceCategory
import scala.io.StdIn.{readInt, readLine}

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
    println("\nFile name:")
    val fileName = readLine()

    try {
      val output = new FileWriter(fileName, false)

      val result = backtrack(genChessboard(horizontal, vertical),
        new PieceState(kings, queens, bishops, rooks, knights),
        output)

      println("\n-----------------\n\nTotal:" + result)

      output.close()

    } catch {
      case e: IOException =>
        println("ERROR writing to file: " + e.getMessage)
      case e: FileNotFoundException =>
        println("ERROR creating file: " + e.getMessage)
    }
  }

  //see if pieces of the same type are placed in lexicographical order on the chessboard
  //(so that we do not count permutations)
  private def arePiecesInOrder(piece1: Piece, piece2: Piece): Boolean =
    piece1.isSameCategory(piece2) && piece1.isAfter(piece2)

  private def placePiece(fields: Seq[(Int, Int)], state: PieceState, newPieceCat: PieceCategory, output: FileWriter): Int =
    fields
      .map((coords: (Int, Int)) => {
        val newPiece = Piece(newPieceCat, coords)
        if (state.piecesOnTheBoard exists ((p: Piece) => newPiece.beats(p) || arePiecesInOrder(newPiece, p)))
          0
        else
          backtrack(fields filter (!newPiece.beats(_)), state.addPieceToTheBoard(newPiece), output)})
      .sum

  private def stateToNextCategory(state: PieceState): Option[PieceCategory] =
    List(Category.QUEEN,
         Category.ROOK,
         Category.BISHOP,
         Category.KING,
         Category.KNIGHT)
      .find(state.piecesLeft(_) != 0)

  // assuming there's available fields & pieces
  private def backtrackStep(fields: Seq[(Int, Int)], state: PieceState, output: FileWriter): Int =
    stateToNextCategory(state)
      .map((cat: PieceCategory) => placePiece(fields, state removePieceFromLeft cat, cat, output))
      .getOrElse(0)

  def genChessboard(horizontal: Int, vertical: Int): Seq[(Int, Int)] =
      for {
        i <- 1 to horizontal
        j <- 1 to vertical
      } yield (i, j)

  def dumpState(pieces: Seq[Piece], output: FileWriter): Unit = {
    val preppedString = pieces mkString ", "
    output write preppedString + "\n"
  }

  def backtrack(fields: Seq[(Int, Int)], state: PieceState, output: FileWriter): Int =
    if (!state.anyPiecesLeft()) {
      dumpState(state.piecesOnTheBoard, output)
      1
    } else if (fields.isEmpty)
      0
    else backtrackStep(fields, state, output)

  class PieceState(val piecesLeft: Map[Piece.Category.PieceCategory, Int],
                   val piecesOnTheBoard: List[Piece] = List(),
                   val count: Int) {
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

    private def decreaseCountOfCategory(n: PieceCategory) = (m: Map[PieceCategory, Int]) =>
      m + (n -> (m(n) - 1))

    private def changePiecesLeft(f: (Map[PieceCategory, Int] => Map[PieceCategory, Int])): Map[PieceCategory, Int] =
      f(piecesLeft)
  }
}
