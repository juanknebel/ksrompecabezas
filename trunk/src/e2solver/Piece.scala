package e2solver

class Piece(var l: Char,var u: Char, var r: Char,var d: Char){
	def up = u;
	def right = r;
	def down = d;
	def left = l;
	
	def rotate : Piece =  {
		val auxLeft = left;
		val auxUp = up;
		val auxRight = right;
		val auxDown = down;
		l = auxDown;
		u = auxLeft;
		r = auxUp;
		d = auxRight;
		
		this;
	}
	
	override def clone : Piece = {
		new Piece(left,up,right,down)
	}
	
	def isCorner : Boolean = {
		(2 == emptyColor);
	}
	
	def isBorder : Boolean = {
		(1 == emptyColor);
	}
	
	private def emptyColor: Int = {
		var count = 0;
		if(up == 'E') count+=1;
		if(down == 'E') count+=1;
		if(right == 'E') count+=1;
		if(left == 'E') count+=1;
		count;
	}
}