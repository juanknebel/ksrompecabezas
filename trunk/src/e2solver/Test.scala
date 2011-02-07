package e2solver

object Test {
  def main(args: Array[String]): Unit =
    {
      val pieces = Array(new Piece('E', 'E', 'B', 'G'), new Piece('B', 'E', 'E', 'R'), new Piece('E', 'G', 'Y', 'E'), new Piece('Y', 'R', 'E', 'E'))
    
      val correctPuzzle = PuzzleSolutionCreater.create(pieces)
      val incorrectPuzzle = PuzzleSolutionCreater.create(correctPuzzle.pieces)
      val incorrectPuzzle2 = PuzzleSolutionCreater.create(correctPuzzle.pieces)
      
      incorrectPuzzle.rotatePiece(0, 1)
      incorrectPuzzle2.swapPieces(0,0,0,1)

      println(PieceSerializer.writer(incorrectPuzzle.pieces))
      println(ObjectiveFunction.eval(incorrectPuzzle))
      
      println(PieceSerializer.writer(incorrectPuzzle2.pieces))
      println(ObjectiveFunction.eval(incorrectPuzzle2))


      println(PieceSerializer.writer(correctPuzzle.pieces))
      println(ObjectiveFunction.eval(correctPuzzle))
    }
}
