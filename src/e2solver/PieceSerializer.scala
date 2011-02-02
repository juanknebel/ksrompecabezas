package e2solver

object PieceSerializer {
	def reader(stream: String) : Array[Piece] = 
		stream.split(";") map (x => createPiece(x split(",")));
	
	def writer(pieces: Array[Piece]) : String = 
		pieces.map(x => x.left + "," + x.up + "," + x.right + "," + x.down )
		.mkString("",";",";");
	
	private def createPiece(parts: Array[String]) : Piece = 
		new Piece(parts(0)(0), parts(1)(0),parts(2)(0),parts(3)(0));
}