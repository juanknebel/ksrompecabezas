package e2solver

object ObjectiveFunction {
	def eval(aPuzzle: PuzzleSolution):Int = {
		var value = 0;
		var tamanio = aPuzzle.dimension
		for (i <- 0 to tamanio - 1;j <- 0 to tamanio - 1){
			lazy val evaluatePosition = positionAtBoard(i,j,tamanio) _ 
			value +=evaluatePosition(aPuzzle)
		}
		value;
	}
	
	private def positionAtBoard(row: Int, col: Int, dimension:Int)(aPuzzle: PuzzleSolution):Int = {
		var value = 0
		if (row == 0 && col == 0) value = evaluateUpperLeftCorner(row, col)(aPuzzle: PuzzleSolution) 
		if (row == 0 && col ==  dimension - 1) value = evaluateUpperRightCorner(row, col)(aPuzzle: PuzzleSolution)
		if (row == dimension - 1 && col == 0) value =  evaluateLowerLeftCorner(row, col)(aPuzzle: PuzzleSolution)
		if (row == dimension - 1 && col == dimension - 1) value =  evaluateLowerRightCorner(row, col)(aPuzzle: PuzzleSolution)
		if (row == 0 && col != 0 && col != dimension - 1) value =  evaluateUpperSide(row, col)(aPuzzle: PuzzleSolution)
		if (row == dimension - 1 && col != 0 && col != dimension - 1) value =  evaluateLowerSide(row, col)(aPuzzle: PuzzleSolution)
		if (col == 0 && row != 0 && row != dimension - 1) value =  evaluateLeftSide(row, col)(aPuzzle: PuzzleSolution)
		if (col == dimension - 1 && row != 0 && row != dimension - 1) value =  evaluateRightSide(row, col)(aPuzzle: PuzzleSolution)
		if (col != 0 && col != dimension - 1 && row != 0 && row != dimension - 1) value =  evaluateMiddle(row, col)(aPuzzle: PuzzleSolution)
		value
	}
	
	private def evaluateUpperLeftCorner(row: Int, col: Int)(aPuzzle:PuzzleSolution): Int = {
		var piece = aPuzzle.piece(row, col)
		var value = 0
		if (piece.up == 'E') value += 1
		if (piece.left == 'E') value += 1
		if (piece.down == aPuzzle.piece(row+1, col).up) value += 1
		if (piece.right == aPuzzle.piece(row, col+1).left) value += 1
		value
	}
	
	private def evaluateUpperRightCorner(row: Int, col: Int)(aPuzzle:PuzzleSolution): Int = {
		var piece = aPuzzle.piece(row, col)
		var value = 0
		if (piece.up == 'E') value += 1
		if (piece.right == 'E') value += 1
		if (piece.down == aPuzzle.piece(row+1, col).up) value += 1
		if (piece.left == aPuzzle.piece(row, col-1).right) value += 1
		value
	}
	
	private def evaluateLowerLeftCorner(row: Int, col: Int)(aPuzzle:PuzzleSolution): Int = {
		var piece = aPuzzle.piece(row, col)
		var value = 0
		if (piece.down == 'E') value += 1
		if (piece.left == 'E') value += 1
		if (piece.up == aPuzzle.piece(row-1, col).down) value += 1
		if (piece.right == aPuzzle.piece(row, col+1).left) value += 1
		value
	}
	
	private def evaluateLowerRightCorner(row: Int, col: Int)(aPuzzle:PuzzleSolution): Int = {
		var piece = aPuzzle.piece(row, col)
		var value = 0
		if (piece.down == 'E') value += 1
		if (piece.right == 'E') value += 1
		if (piece.up == aPuzzle.piece(row-1, col).down) value += 1
		if (piece.left == aPuzzle.piece(row, col-1).right) value += 1
		value
	}
	
	private def evaluateUpperSide(row: Int, col: Int)(aPuzzle:PuzzleSolution): Int = {
		var piece = aPuzzle.piece(row, col)
		var value = 0
		if (piece.up == 'E') value += 1
		if (piece.right == aPuzzle.piece(row, col+1).left) value += 1
		if (piece.down == aPuzzle.piece(row+1, col).up) value += 1
		if (piece.left == aPuzzle.piece(row, col-1).right) value += 1
		value
	}
	
	private def evaluateLowerSide(row: Int, col: Int)(aPuzzle:PuzzleSolution): Int = {
		var piece = aPuzzle.piece(row, col)
		var value = 0
		if (piece.down == 'E') value += 1
		if (piece.right == aPuzzle.piece(row, col+1).left) value += 1
		if (piece.up == aPuzzle.piece(row-1, col).down) value += 1
		if (piece.left == aPuzzle.piece(row, col-1).right) value += 1
		value
	}
	
	private def evaluateLeftSide(row: Int, col: Int)(aPuzzle:PuzzleSolution): Int = {
		var piece = aPuzzle.piece(row, col)
		var value = 0
		if (piece.down == aPuzzle.piece(row+1, col).up) value += 1
		if (piece.left == 'E') value += 1
		if (piece.up == aPuzzle.piece(row-1, col).down) value += 1
		if (piece.right == aPuzzle.piece(row, col+1).left) value += 1
		value
	}
	
	private def evaluateRightSide(row: Int, col: Int)(aPuzzle:PuzzleSolution): Int = {
		var piece = aPuzzle.piece(row, col)
		var value = 0
		if (piece.down == aPuzzle.piece(row+1, col).up) value += 1
		if (piece.right == 'E') value += 1
		if (piece.up == aPuzzle.piece(row-1, col).down) value += 1
		if (piece.left == aPuzzle.piece(row, col-1).right) value += 1
		value
	}
	
	private def evaluateMiddle(row: Int, col: Int)(aPuzzle:PuzzleSolution): Int = {
		var piece = aPuzzle.piece(row, col)
		var value = 0
		if (piece.down == aPuzzle.piece(row+1, col).up) value += 1
		if (piece.right == aPuzzle.piece(row, col+1).left) value += 1
		if (piece.up == aPuzzle.piece(row-1, col).down) value += 1
		if (piece.left == aPuzzle.piece(row, col-1).right) value += 1
		value
	}
}
