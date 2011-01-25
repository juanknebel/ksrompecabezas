package core
import java.io.File
import java.io.FileWriter
import java.io.PrintWriter
import java.awt.Color
import collection.mutable.ArrayBuffer
import util.Random

object e2Solver{
	def main(args: Array[String]) {
		new PuzzleBoardWriter(new Puzzle(scala.io.Source.fromFile("tablero.in").mkString).pieces).writeOn(new File("tablero.out"));
	}
}

class Puzzle(board: String) {
	def pieces : Array[Piece] = board.split(";") map (x => createPiece(x));
  
	def createPiece(sides: String):Piece = {
		val pieces = sides split(",");
		new Piece(pieces(0), pieces(1),pieces(2),pieces(3));
	}
}

class PuzzleBoardWriter(private val pieces:Array[Piece]) {
    def writeOn(file:File):Unit = {
        val out = new PrintWriter(file)
        try {
            writeOn(out)
        } finally {
            out.close()
        }
    }

    def writeOn(out:PrintWriter):Unit = pieces.foreach(writePiece(out, _))
    
    private def writePiece(out:PrintWriter, piece:Piece) =
        out.print(piece.parts.map(_.toString).mkString("", ",", ";"))
}

class Piece(u: String, r: String, d: String, l: String){
	def up = u;
	def right = r;
	def down = d;
	def left = l;
	
	def parts = List(l,u,r,d).toBuffer;
}