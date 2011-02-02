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
}