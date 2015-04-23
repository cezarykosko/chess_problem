object ChessProblem {

  final private val KINGS = "K"
  final private val QUEENS = "Q"
  final private val BISHOPS = "B"
  final private val ROOKS = "R"
  final private val KNIGHTS = "Kn"

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

  class PieceState(val piecesLeft: Map[String, Int], val piecesOnTheBoard: List[Piece] = List()) {
    def anyPiecesLeft(): Boolean =
      List(KINGS, QUEENS, BISHOPS, ROOKS, KNIGHTS).exists(piecesLeft(_) != 0)

    def this(kings: Int, queens: Int, bishops: Int, rooks: Int, knights: Int) =
      this(Map(KINGS -> kings, QUEENS -> queens, BISHOPS -> bishops, ROOKS -> rooks, KNIGHTS -> knights), List())

    def addPieceToTheBoard(piece: Piece): PieceState = new PieceState(piecesLeft, piece :: piecesOnTheBoard)

    def changePiecesLeft(f: (Map[String, Int] => Map[String, Int])): PieceState =
      new PieceState(f(piecesLeft), piecesOnTheBoard)
  }

  //TODO: rename
  private def isAfter(piece1: Piece, piece2: Piece): Boolean =
    piece1.getClass == piece2.getClass && isAfter(piece1.coords, piece2.coords)

  private def isAfter(coord1: (Int, Int), coord2: (Int, Int)): Boolean =
    coord1._1 > coord2._1 || (coord1._1 == coord2._1 && coord1._2 > coord2._2)

  private def remove(n: String) = (m: Map[String, Int]) => m + (n -> (m(n) - 1))

  private def place_piece(fields: Seq[(Int, Int)], state: PieceState, newPiece: Piece): Int = {
    (for {
      coords <- fields
    } yield {
        newPiece.coords = coords
        if (state.piecesOnTheBoard exists ((p: Piece) => newPiece.beats(p) || isAfter(newPiece, p)))
          0
        else
          backtrack(fields filter (!newPiece.beats(_)), state.addPieceToTheBoard(newPiece))
      }).sum
  }

  // assuming there's available fields & pieces
  private def backtrack_step(fields: Seq[(Int, Int)], state: PieceState): Int = {
    if (state.piecesLeft(QUEENS) != 0)
      place_piece(fields, state changePiecesLeft remove(QUEENS), new Queen)
    else if (state.piecesLeft(ROOKS) != 0)
      place_piece(fields, state changePiecesLeft remove(ROOKS), new Rook)
    else if (state.piecesLeft(BISHOPS) != 0)
      place_piece(fields, state changePiecesLeft remove(BISHOPS), new Bishop)
    else if (state.piecesLeft(KINGS) != 0)
      place_piece(fields, state changePiecesLeft remove(KINGS), new King)
    else if (state.piecesLeft(KNIGHTS) != 0)
      place_piece(fields, state changePiecesLeft remove(KNIGHTS), new Knight)
    else
      1
  }

  private def backtrack(fields: Seq[(Int, Int)], state: PieceState): Int = {
    //println(fields)
    if (!state.anyPiecesLeft())
      1
    else if (fields.isEmpty)
      0
    else backtrack_step(fields, state)
  }
}

