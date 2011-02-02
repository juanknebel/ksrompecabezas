package e2solver

object Test {
  def main(args : Array[String]) : Unit = 
  {
	  var correctPuzzle = Array(new Piece('E','E','B','G'),new Piece('B','E','E','R'),new Piece('E','G','Y','E'),new Piece('Y','R','E','E'));
	  println(ObjectiveFunction.eval(correctPuzzle));
	  
	  val s = correctPuzzle(0); 
	  correctPuzzle(0) = correctPuzzle(1);
	  correctPuzzle(1) = s;
	  correctPuzzle(0).rotate.rotate.rotate;
	  correctPuzzle(1) rotate; 
	  println(ObjectiveFunction.eval(correctPuzzle));
	     
	     
	   
  }
}
