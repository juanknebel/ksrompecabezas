package e2solver

class PuzzleSolution(var iPieces: Array[Piece]) {
  def pieces = iPieces;

  def dimension: Int = math.sqrt(pieces.length).toInt;

  private def piecePosition(x: Int, y: Int): Int = x * dimension + y;

  def piece(x: Int, y: Int): Piece = pieces(piecePosition(x, y));

  def rotatePiece(x: Int, y: Int) = iPieces(piecePosition(x, y)) = iPieces(piecePosition(x, y)).rotate;
  
  def swapPieces (x1: Int, y1: Int, x2: Int, y2: Int) =
  {
	  val temp = iPieces(piecePosition(x1,y1))
	  iPieces(piecePosition(x1,y1)) = iPieces(piecePosition(x2,y2))
	  iPieces(piecePosition(x2,y2)) = temp
  }
  
  override def clone : PuzzleSolution = {
	  new PuzzleSolution(pieces.clone);
  }
}

object PuzzleSolutionCreater {
  def create(pieces: Array[Piece]): PuzzleSolution =

    {
      var newPieces = new Array[Piece](pieces.size);
      for (i <- 0 to pieces.size - 1) {
        newPieces(i) = pieces(i).clone;
      }

      new PuzzleSolution(newPieces)
    }
}

object TestPuzzleSolution {
	def main(args: Array[String]): Unit = {
		testIntercambiarYRotar
	}
	
	private def testIntercambiarYRotar: Unit = {
		var pieces: Array[Piece] = Array(new Piece('E', 'E', 'B', 'G'), new Piece('B', 'E', 'E', 'R'), new Piece('E', 'G', 'Y', 'E'), new Piece('Y', 'R', 'E', 'E'))
		val correctPuzzle = PuzzleSolutionCreater.create(pieces)
		val incorrectPuzzle = PuzzleSolutionCreater.create(correctPuzzle.pieces)
		val incorrectPuzzle2 = PuzzleSolutionCreater.create(correctPuzzle.pieces)
		
		incorrectPuzzle.rotatePiece(0, 1)
		incorrectPuzzle2.swapPieces(0,0,0,1)
		
		println("Puzzle incorrecto 1")
		println(PieceSerializer.writer(incorrectPuzzle.pieces))
		println(ObjectiveFunction.eval(incorrectPuzzle))
		
		println("Puzzle incorrecto 2")
		println(PieceSerializer.writer(incorrectPuzzle2.pieces))
		println(ObjectiveFunction.eval(incorrectPuzzle2))
		
		println("Puzzle correcto")
		println(PieceSerializer.writer(correctPuzzle.pieces))
		println(ObjectiveFunction.eval(correctPuzzle))
	}
}