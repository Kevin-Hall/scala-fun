

import hw.tictactoe._

class Game(val turn: Option[Player] , dim: Int , board: Map[(Int , Int), Player ]) extends  GameLike[Game] {
	def  isFinished ():  Boolean = {
		if (getWinner() == None){

			val boardsize = dim * dim
			(board.size == boardsize)
			
		} else {
			true
		}
	}
	/*  Assume  that  isFinished  is true */
	def  getWinner ():  Option[Player] = {
		if (board.isEmpty){None}else{
		if (horizontals(dim-1,dim-1,board) != None){ 
				horizontals(dim-1,dim-1,board)
			} else if (verticals(dim-1,dim-1,board) != None){
				verticals(dim-1,dim-1,board)
			} else if (checkDiagonalLeft(dim-1,dim-1,board) != None){
				checkDiagonalLeft(dim-1,dim-1,board)
			} else {
				checkDiagonalRight(0,dim-1,board)
			}

		}
		
	}

	//getwinner Helpers
	/*------------------------------------------------------------------------------- */

	def horizontals(x: Int, y: Int, board: Map[(Int , Int), Player ]):Option[Player] = {
		if (board.isEmpty){None}else{
		if (y == 0){
				if (checkHorizontal(x,y,board)) {board.get(x,y)} else {None}
			} else {
				if (checkHorizontal(x,y,board)) {board.get(x,y)} else {horizontals(x,y-1,board)}
			}
		}
	}

	def checkHorizontal ( x: Int,y: Int, board: Map[(Int , Int), Player ]) :Boolean = {
		if (board.isEmpty){false}else{
		if (x == 0){true} //returns the winner //Option(board.get(x-1,y).get)
		else {
			if (board.get(x,y) == board.get(x-1,y)){
				checkHorizontal(x-1,y,board) // traverse left
			}else{
				{false} // if the player in the next pos isnt the same 
			}
		}
		}
	}

	def verticals(x: Int, y: Int, board: Map[(Int , Int), Player ]):Option[Player] = {
		if (x == 0){
				if (checkVertical(x,y,board)) {board.get(x,y)} else {None}
			} else {
				if (checkVertical(x,y,board)) {board.get(x,y)} else {verticals(x-1,y,board)}
			}
	}

	def checkVertical (x: Int,y: Int, board: Map[(Int , Int), Player ]) :Boolean  = {
		if (board.isEmpty){false}else{
		if (y == 0){true} //returns the winner //Option(board.get(x-1,y).get)
			else {
				if (board.get(x,y) == board.get(x,y-1)){
					checkVertical(x,y-1,board) // traverse left
				}else{
					{false} // if the player in the next pos isnt the same 
				}
			}
		}
		}

	def checkDiagonalLeft (x: Int,y: Int, board: Map[(Int , Int), Player ]) :Option[Player] = {
		if (board.isEmpty){None}else{
		if (y == 0){board.get(x,y)} //returns the winner //Option(board.get(x-1,y).get)
				else {
					if (board.get(x,y) == board.get(x-1,y-1)){
						checkDiagonalLeft(x-1,y-1,board) // traverse left
					}else{
						{None} // if the player in the next pos isnt the same 
					}
				}
			}
		}

	def checkDiagonalRight (x: Int,y: Int, board: Map[(Int , Int), Player ]) :Option[Player] = {
		if (board.isEmpty){None}else{
		if (y == 0){board.get(x,y)} //returns the winner //Option(board.get(x-1,y).get)
			else {
				if (board.get(x,y) == board.get(x+1,y-1)){
					checkDiagonalRight(x+1,y-1,board) // traverse left
				}else{
					{None} // if the player in the next pos isnt the same 
				}
			}
		}
		}





	def  nextBoards ():  List[Game] = {
		if (turn == Some(X)){
			Players(List(),X,dim-1,dim-1,board)
		} else {
			Players(List(),O,dim-1,dim-1,board)
		}
	}


	//nextboards Helpers
	/*------------------------------------------------------------------------------- */

	def Players(lst: List[Game], player: Player, x: Int, y: Int, board: Map[(Int , Int), Player ]): List[Game] = {
		if (y == 0){lst}
			else {
				 Players(lst ::: determinePlayer(lst,player,x,y-1,board),player,x,y-1,board)
			}
		
	}

	def determinePlayer (lst: List[Game], player: Player, x: Int,y: Int, board: Map[(Int , Int), Player ]) :List[Game] = {
		if (x == 0){lst} //returns the winner //Option(board.get(x-1,y).get)
		else {
			if (board.get(x,y) == None){
				val newList = List(Solution.createGame(player,dim,board + ((x,y) -> player)))//creates a game with a player in the given coordinates 
				determinePlayer(lst ::: newList, player, x-1,y,board) // traverse left
			}else{
				determinePlayer(lst,player,x-1,y,board) // traverse left
			}
		}
	}


}

object  Solution  extends  MinimaxLike {
	type T = Game // T is an "abstract  type  member" of  MinimaxLike
	
	def  createGame(turn: Player, dim: Int , board: Map[(Int , Int), Player ]):  Game = {
		new Game(Some(turn),dim,board)
	}
	


def  minimax(board: Game): Option[Player] = {
	if (board.turn == Some(X)){
		if (board.getWinner() == Some(X)){
			Some(X)
		} else{
			if (board.isFinished){
				None
			}else{
				val lst = board.nextBoards().map{x => minimax(x)}
				if (lst.contains(Some(O))){Some(O)}
					else if (lst.contains(Some(X))){Some(X)}
					else{None}
			}
		}
	} 

	else{
		if (board.getWinner() == Some(O)){
			Some(O)
		} else{
			if (board.isFinished){
				None
			}else{
				val lst2 = board.nextBoards().map{y => minimax(y)}
				if (lst2.contains(Some(O))){Some(O)}
					else if (lst2.contains(Some(X))){Some(X)}
					else{None}
			}
		}
	}
}
}








