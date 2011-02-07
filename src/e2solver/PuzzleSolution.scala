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