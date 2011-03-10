package e2solver

object Test {
	private def fileIn: String = "tablero.in";
	private def fileOut: String = "tablero.out";
	private def fileOrig: String = "tablero_resuelto.txt";

	def main(args: Array[String]): Unit = {
		val puzzleResolved = PuzzleSolutionCreater.create(PuzzleSolutionReader.readOn(fileOrig))
		println("El puntaje optimo es: "+ObjectiveFunction.eval(puzzleResolved))
		println("Leyendo el puzzle inicial")
		val aPuzzle = PuzzleSolutionCreater.create(PuzzleSolutionReader.readOn(fileIn))
		println("El puntaje es: "+ObjectiveFunction.eval(aPuzzle))
		var aSolution = BeeAlgorithm.run(aPuzzle)
		println("El puntaje final es: "+ObjectiveFunction.eval(aSolution))
		println("Escribiendo la solucion")
		PuzzleSolutionWriter.writeOn(fileOut, aSolution)
	}
}
