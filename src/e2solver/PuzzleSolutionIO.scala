package e2solver

import java.io.File
import java.io.FileWriter
import java.io.PrintWriter

object PuzzleSolutionWriter {
	def writeOn(name: String, aPuzzle: PuzzleSolution): Unit = {
		var file = new File(name)
		val out = new PrintWriter(file)
		try {
			writeOn(out, aPuzzle)
		}
		finally {
			out.close()
		}
	}
	
	def writeOn(out: PrintWriter, aPuzzle: PuzzleSolution): Unit = out.print(PieceSerializer.writer(aPuzzle.pieces))
}

object PuzzleSolutionReader {
	def readOn(name: String): Array[Piece] = {
		val lines = scala.io.Source.fromFile(name).mkString
		PieceSerializer.reader(lines)
	}
}

object TestIO {
	def main(args: Array[String]): Unit = {
		testLeerYEscribir;
	}
  
	private def testLeerYEscribir: Unit = {
		println("Leyendo...")
		val aPuzzle = PuzzleSolutionCreater.create(PuzzleSolutionReader.readOn("tablero.in"))
		println("Escribiendo...")
		PuzzleSolutionWriter.writeOn("tablero.out", aPuzzle)
	}
}