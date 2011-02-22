package e2solver

object BeeAlgorithm {
	private def maxIteration = 100000000;
	private def maxIterationWithoutChange = 1000;
	private def localSearchIteration = 100;
	private def employed = 100;
	private def onLooker = 15;
	private def scout = 10; 
	
	def run (puzzleSolution: PuzzleSolution): PuzzleSolution = {
		var initialSolutions = generateRandomSolutions(employed, puzzleSolution);
		var energyValueSolutions = evaluateAllEnergyValue(initialSolutions);
		var i = 0;
		var j = 0;
		var savedSolutions = new List[puzzleSolution]();
		while (i < maxIteration || j < maxIterationWithoutChange) {
			//recluto las abejas onlookers
			var onLookersSolutions = onLookerChoosingStrategy(initialSolutions, energyValueSolutions);
		
			//para cada onlooker busco una solucion vecina
			for (k<-0 to onLooker - 1) {
				var rand = new scala.util.Random();
				var initialLocalEnergy = ObjectiveFunction.eval(onLookersSolutions(k));
				var initialLocalSolution = onLookersSolutions(k);
				var maxLocalEnergy = initialEnergy;
				var maxLocalSolution = initialLocalSolution;
				var tempSolution = onLookersSolutions(k);
				for (l<-0 to localSearchIteration - 1) {
					tempSolution = localSearchStrategy(tempSolution);
					var tempEnergy = ObjectiveFunction.eval(tempSolution);
					if (tempEnergy > maxLocalEnergy) {
						maxLocalEnergy = tempEnergy;
						maxLocalSolution = tempSolution;
					}
					
					if (tempEnergy < initialLocalEnergy && rand.nextInt(1) > 0.05) {
						temp =  initialSolution;
					}						
				}
				if (maxLocalEnergy == initialLocalEnergy) {
					savedSolutions ::= onLookersSolutions(k);
					onLookersSolutions(k) = scoutSearchSolution(onLookersSolutions(k));
				}
				else {
					onLookersSolutions(k) = maxLocalSolution;
				}
			}
			replaceWorstSolutions(initialSolutions, energyValueSolutions, onLookersSolutions);
		}
		initialSolutions(0);
	}
	
	private def replaceWorstSolutions(oldSolutions: Array[PuzzleSolution], energyOldSolutions: Array[Int],
			newSolutions: Array[PuzzleSolution]) = {
		for (i<-0 to newSolutions) {
			//TODO
		}
	}
	
	private def localSearchStrategy(aSolution: PuzzleSolution): PuzzleSolution = {
		newSolution = aSolution.clone;
		//TODO:cambiar piezas de la file/columna que tenga peor funcion parcial
		var rand = new scala.util.Random();
		var dimension = sSolution.dimension;
		
		newSolution.swap(rand.nextInt(dimension), rand.nextInt(dimension), 
				rand.nextInt(dimension), rand.nextInt(dimension));
		newSolution.swap(rand.nextInt(dimension), rand.nextInt(dimension), 
				rand.nextInt(dimension), rand.nextInt(dimension));
		
		newSolution.rotatePiece(rand.nextInt(dimension), rand.nextInt(dimension));
		newSolution.rotatePiece(rand.nextInt(dimension), rand.nextInt(dimension));
		newSolution;
	}
	
	private def onLookerChoosingStrategy(aSolutions: Array[PuzzleSolution], energyValueSolutions: Array[Double]):
			Array[PuzzleSolution] = {
		var onLookersSolutions = new Array[PuzzleSolution](onLooker);
		var fitnessValue = new Array[Double](employed);
		var rand = new scala.util.Random();
		for (i<-0 to employed - 1) {
			fitnessValue(i) = rand.nextInt(1) * (1/energyValueSolutions(i));
		}
		//TODO:filtrar por el valor de la energia
		for (j<-0 to onLooker - 1) {
			onLookersSolutions(j) = aSolutions(j);
		}
		onLookersSolutions;
	}
	
	private def evaluateAllEnergyValue(aSolution: PuzzleSolution): Array[Double] = {
		var energyValue = new Array[Double](aSolution.size);
		for (i<- 0 to aSolution.size - 1) {
			energyValue(i) = ObjectiveFunction.eval(initialSolutions(i));
		}
		energyValue;
	}
	
	private def generateRandomSolutions(employed: Int, aSolution: PuzzleSolution): Array[PuzzleSolution] = {
		var solutions = new Array[PuzzleSolution](employed);
		for (i <- 0 to employed - 1) {
			solutions(i) = scoutSearchSolution(aSolution)
		}
		solutions;
	}
	
	private def scoutSearchSolution(aSolution: PuzzleSolution): PuzzleSolution {
		var rand = new scala.util.Random();
		var dimension = aSolution.dimension;
		
		var newSolution = aSolution.clone;
		
		for(i <- 0 to dimension - 1) {
			for(j <- 0 to dimension - 1) {
				newSolution.swapPieces(i,j,rand.nextInt(dimension), rand.nextInt(dimension));
				if (rand.nextInt(1) < 0.5) {
					newSolution.rotatePiece(rand.nextInt(dimension), rand.nextInt(dimension));
				}
			}
		}
		newSolution;
	}
}