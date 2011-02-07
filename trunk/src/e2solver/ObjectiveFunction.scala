package e2solver


object ObjectiveFunction {
	def eval(puzzleSolution: PuzzleSolution):Int = {
		var value = 0;
		for (i <- 0 to puzzleSolution.dimension - 1){
			for (j <- 0 to puzzleSolution.dimension - 1){
				if (j<puzzleSolution.dimension - 1 && puzzleSolution.piece(i,j).right != puzzleSolution.piece(i,j+1).left){
					value += 1;
				}
				
				if(i>0 && puzzleSolution.piece(i,j).up != puzzleSolution.piece(i-1,j).down){
					value += 1;
				}
			}
		}
		
		value;
	}
}