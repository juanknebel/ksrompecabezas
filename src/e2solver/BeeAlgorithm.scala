package e2solver

object BeeAlgorithm {
	private def maxIteration = 10000;
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
		var savedSolutions = List[PuzzleSolution]();
		//while (i < maxIteration || j < maxIterationWithoutChange) {
		while (i < maxIteration) {
			//recluto las abejas onlookers
			var onLookersSolutions = onLookerChoosingStrategy(initialSolutions, energyValueSolutions);
		
			//para cada onlooker busco una solucion vecina
			for (k<-0 to onLooker - 1) {
				var rand = new scala.util.Random();
				var initialLocalEnergy = ObjectiveFunction.eval(onLookersSolutions(k));
				var initialLocalSolution = onLookersSolutions(k);
				var maxLocalEnergy = initialLocalEnergy;
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
						tempSolution =  initialLocalSolution;
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
			//reemplazo las peores soluciones del inicial con las que generaron las onlookers
			replaceWorstSolutions(initialSolutions, energyValueSolutions, onLookersSolutions);
			i += 1;
		}
		
		maxSolution(initialSolutions)
		 
	}
	
	private def maxSolution(solutions: Array[PuzzleSolution]):PuzzleSolution = {
		var max = 0;
		var energySolutions = evaluateAllEnergyValue(solutions);
		for (i<-0 to solutions.size - 1) {
			if (energySolutions(max)<energySolutions(i)){
				max = i;		
			}
		}
		
		solutions(max);
	}
	
	private def replaceWorstSolutions(oldSolutions: Array[PuzzleSolution], energyOldSolutions: Array[Double],
			newSolutions: Array[PuzzleSolution]) = {
		
		var energyNewSolutions = evaluateAllEnergyValue(newSolutions);
		
		class SolutionEnergy(_solution: PuzzleSolution, _energy: Double){
			def solution = _solution;
			def energy = _energy;
		}
		
		var newSolutionsEnergy = new Array[SolutionEnergy](newSolutions.size);
		var oldSolutionsEnergy = new Array[SolutionEnergy](oldSolutions.size);
		
		for (i<-0 to newSolutions.size - 1) {
			newSolutionsEnergy(i) = new SolutionEnergy(newSolutions(i),energyNewSolutions(i));
		}
		
		for (i<-0 to oldSolutions.size - 1) {
			oldSolutionsEnergy(i) = new SolutionEnergy(oldSolutions(i),energyOldSolutions(i));
		}
		
		scala.util.Sorting.stableSort(newSolutionsEnergy, (s1:SolutionEnergy, s2:SolutionEnergy) => s1.energy>s2.energy);
		scala.util.Sorting.stableSort(oldSolutionsEnergy, (s1:SolutionEnergy, s2:SolutionEnergy) => s1.energy>s2.energy);
		
		var oldPos = 0;
		var newPos = 0;
		for (i<-0 to oldSolutions.size - 1) {
			if (newPos < newSolutionsEnergy.size && newSolutionsEnergy(newPos).energy>oldSolutionsEnergy(oldPos).energy){
				oldSolutions(i) = newSolutionsEnergy(newPos).solution;
				newPos = newPos+1;
			}
			else{
				oldSolutions(i) = oldSolutionsEnergy(oldPos).solution;
				oldPos = oldPos+1;
			}		
		}		
	}
	
	private def localSearchStrategy(aSolution: PuzzleSolution): PuzzleSolution = {
		var newSolution = aSolution.clone;
		//TODO:posible mejora cambiar piezas de la fila/columna que tenga peor funcion parcial
		var rand = new scala.util.Random();
		var dimension = aSolution.dimension;
		
		newSolution.swapPieces(rand.nextInt(dimension), rand.nextInt(dimension), 
				rand.nextInt(dimension), rand.nextInt(dimension));
		newSolution.swapPieces(rand.nextInt(dimension), rand.nextInt(dimension), 
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
	
	private def evaluateAllEnergyValue(aSolutions: Array[PuzzleSolution]): Array[Double] = {
		var energyValue = new Array[Double](aSolutions.size);
		for (i<- 0 to aSolutions.size - 1) {
			energyValue(i) = ObjectiveFunction.eval(aSolutions(i));
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
	
	private def scoutSearchSolution(aSolution: PuzzleSolution): PuzzleSolution = {
		//TODO:posible mejora primero ubicar todas piezas que son esquina en las esquinas
		var rand = new scala.util.Random();
		var dimension = aSolution.dimension;
		
		var newSolution = aSolution.clone;
		
		for (i <- 0 to dimension - 1) {
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

object TestBCOAlgorithm {
	def main(args: Array[String]): Unit = {
		testAlgoritmoTableroChico
	}
	
	private def testAlgoritmoTableroChico: Unit = {
		var pieces: Array[Piece] = Array(new Piece('E', 'E', 'B', 'G'), new Piece('B', 'E', 'E', 'R'), new Piece('E', 'G', 'Y', 'E'), new Piece('Y', 'R', 'E', 'E'))
		val aPuzzle = PuzzleSolutionCreater.create(pieces)
		aPuzzle.rotatePiece(0, 1)
		aPuzzle.swapPieces(0, 0, 0, 1)
		println("El tablero de entrada")
		println(PieceSerializer.writer(aPuzzle.pieces))
		println(ObjectiveFunction.eval(aPuzzle))

		val thePuzzleSolution = BeeAlgorithm.run(aPuzzle)
		
		println("El tablero de salida")
		println(PieceSerializer.writer(thePuzzleSolution.pieces))
		println(ObjectiveFunction.eval(thePuzzleSolution))
	}
}
