import java.io.FileWriter

import org.scalatest.FunSuite
import ChessProblem._

class BTSuite extends FunSuite {
  val output = new FileWriter("/dev/null", false)
  test("8 queens puzzle") {
    assert(ChessProblem.backtrack(genChessboard(8, 8), new PieceState(0, 8, 0, 0, 0), output).equals(92))
  }

  test("2 rooks on 2x2") {
    assert(ChessProblem.backtrack(genChessboard(2, 2), new PieceState(0, 0, 0, 2, 0), output).equals(2))
  }
  test("2 bishops on 2x2") {
    assert(ChessProblem.backtrack(genChessboard(2, 2), new PieceState(0, 0, 2, 0, 0), output).equals(4))
  }
  test("1 king on 2x2") {
    assert(ChessProblem.backtrack(genChessboard(2, 2), new PieceState(1, 0, 0, 0, 0), output).equals(4))
  }
  test("1 knight on 2x2") {
    assert(ChessProblem.backtrack(genChessboard(2, 2), new PieceState(0, 0, 0, 0, 1), output).equals(4))
  }
  test("1 queen on 2x2") {
    assert(ChessProblem.backtrack(genChessboard(2, 2), new PieceState(0, 1, 0, 0, 0), output).equals(4))
  }
  test("3 rooks on 2x2") {
    assert(ChessProblem.backtrack(genChessboard(2, 2), new PieceState(0, 0, 0, 3, 0), output).equals(0))
  }
  test("3 bishops on 2x2") {
    assert(ChessProblem.backtrack(genChessboard(2, 2), new PieceState(0, 0, 3, 0, 0), output).equals(0))
  }
  test("2 kings on 2x2") {
    assert(ChessProblem.backtrack(genChessboard(2, 2), new PieceState(2, 0, 0, 2, 0), output).equals(0))
  }
}
