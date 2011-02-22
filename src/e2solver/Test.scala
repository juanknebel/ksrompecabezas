package e2solver

object Test {
	
	private def pieces: Array[Piece] = Array(new Piece('E', 'E', 'B', 'G'), new Piece('B', 'E', 'E', 'R'), new Piece('E', 'G', 'Y', 'E'), new Piece('Y', 'R', 'E', 'E'))

	def main(args: Array[String]): Unit = {
	  //test1;
      test2;
    }
  
  private def test1: Unit = {
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
  
  private def test2: Unit = {
	  val aPuzzle = PuzzleSolutionCreater.create(pieces)
	  aPuzzle.rotatePiece(0, 1)
	  aPuzzle.swapPieces(0, 0, 0, 1)
	  println(PieceSerializer.writer(aPuzzle.pieces))
      println(ObjectiveFunction.eval(aPuzzle))
      
	  val thePuzzleSolution = BeeAlgorithm.run(aPuzzle)
	  println(PieceSerializer.writer(thePuzzleSolution.pieces))
      println(ObjectiveFunction.eval(thePuzzleSolution))
  }
}
