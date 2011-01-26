package e2solver

class Piece(var l: String,var u: String, var r: String,var d: String){
	def up = u;
	def right = r;
	def down = d;
	def left = l;
	
	def rotate {
		val auxLeft = left;
		val auxUp = up;
		val auxRight = right;
		val auxDown = down;
		l = auxDown;
		u = auxLeft;
		r = auxUp;
		d = auxDown;
	}
}