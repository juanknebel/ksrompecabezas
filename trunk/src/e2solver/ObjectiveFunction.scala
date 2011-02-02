package e2solver


object ObjectiveFunction {
	def eval(pieces: Array[Piece]):Int = {
		val dimension = math.sqrt(pieces.length).toInt;
		var value = 0;
		for (i <- 0 to (pieces.length - 1)){
			if (i < pieces.length-1 && (pieces(i).right != pieces(i+1).left)){
				value += 1;
			}
			if (i>dimension && (pieces(i).up != pieces(i-dimension).down)){
				value += 1; 
			}
		}
		
		value;
	}
}