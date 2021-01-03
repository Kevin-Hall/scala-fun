import  hw.sudoku._

object  Solution  extends  SudokuLike {
	type T = Board

	val p1 = "..3.2.6..9..3.5..1..18.64....81.29..7.......8..67.82....26.95..8..2.3..9..5.1.3.." 
	val p2 = "2...8.3...6..7..84.3.5..2.9...1.54.8.........4.27.6...3.1..7.4.72..4..6...4.1...3" //
	val p3 = ".3..5..4...8.1.5..46.....12.7.5.2.8....6.3....4.1.9.3.25.....98..1.2.6...8..6..2." //
	val p4 = ".2.81.74.7....31...9...28.5..9.4..874..2.8..316..3.2..3.27...6...56....8.76.51.9."
	val p5 = ".43.8.25.6.............1.949....4.7....6.8....1.2....382.5.............5.34.9.71." //
	val p6 = "48...69.2..2..8..19..37..6.84..1.2....37.41....1.6..49.2..85..77..9..6..6.92...18"
	val empty = "................................................................................."

	def  parse(str: String ):  Board = {

		val emptyBoard = new Board(0.to(8).map(x => 0.to(8).map(y => (x,y) -> List(1,2,3,4,5,6,7,8,9))).flatten.toMap)

		parseHelp(str,0,0,0,emptyBoard)
		
	}

	def parseHelp(str: String,strIndex: Int, row: Int, col:Int, b:Board):Board = {
		if (strIndex == 81){
			b
		}else{
			if (str.charAt(strIndex) == '.'){
				if (col == 8){
					parseHelp(str,strIndex+1,row+1,0,b)
				}else{
					parseHelp(str,strIndex+1,row,col+1,b)
				}
			} else {
				val newBoard = new Board((b.available - ((row,col))) + ((row,col) -> List(str.charAt(strIndex).asDigit))) 
				//val newBoardTwo = new Board(removePeers(str.charAt(strIndex).asDigit,peers(row,col),b.available)) //remove current str.charAt from peers
				val newBoardTwo = new Board(removePeers(str.charAt(strIndex).asDigit,peers(row,col),newBoard.available))
				if (col == 8){
					parseHelp(str,strIndex+1,row+1,0,newBoardTwo)
				}else{
					parseHelp(str,strIndex+1,row,col+1,newBoardTwo)
				}
			}
		}
	}


	def removePeers(toRemove: Int,lst: List[(Int , Int)],map: Map[(Int , Int), List[Int ]]): Map[(Int , Int), List[Int ]] = {
		
		if (lst.length < 1){
			map
		} else{
			val ((row, col) :: rest) = lst

			
			//val row
			//val col
			//val rest - the rest of peers list
			val currMap = map((row,col)) // the current maps list
			//print("map: " + lst.toString)

			if (currMap.length == 1){
				removePeers(toRemove, rest, (map - ((row,col))) + ((row,col) -> currMap.filter(_ != toRemove)))
			} else {
				val newList = currMap.filter(_ != toRemove)
				if ((currMap.filter(_ != toRemove)).length == 1){
					//println("map: " + currMap.toString)
					//println("remove: " + toRemove)
					//println("new peers: " + peers(row,col).toString)
					val newRemove = currMap.filter(_ != toRemove)(0)

					val newMap = removePeers(newRemove, peers(row,col), (map - ((row,col))) + ((row,col) -> (List[Int](newRemove))))
					removePeers(toRemove, rest, newMap)	

				} else {
					removePeers(toRemove, rest, (map - ((row,col))) + ((row,col) -> currMap.filter(_ != toRemove)))				
				}
			}
		}
	}



	// You  can  use a Set  instead  of a List (or, any  Iterable)
	def  peers(row: Int , col: Int): List[(Int , Int)] = {
		//println("row: " + row + " Col: " + col)
		val rows = addRows(row,0,col,Nil) 
		val cols = addCols(col,row,0,rows)
		val cube = addCube(row,col)
		(cols.diff(cube) ::: cube.diff(List((row,col))))
		
	}

	def addRows(given: Int,row: Int , col: Int ,lst: List[(Int , Int)]) : List[(Int , Int)] = row match {
		case 9 => lst
		case `given` => addRows(given,row+1,col,lst)
		case _ => addRows(given,row+1,col,lst ++ List((row,col)))
	}

	def addCols(given: Int,row: Int , col: Int ,lst: List[(Int , Int)]) : List[(Int , Int)] = col match {
		case 9 => lst
		case `given` => addCols(given,row,col+1,lst)
		case _ => addCols(given,row,col+1,lst ++ List((row,col)))
	}

	def addCube(row: Int , col: Int) : List[(Int , Int)] = {
		
		val cubeX = (row / 3)
		val cubeY = (col / 3)

		cubeX match {
			case 0 => cubeY match {
				case 0 => List((cubeX,cubeY),(cubeX+1,cubeY),(cubeX+2,cubeY),(cubeX,cubeY+1),(cubeX,cubeY+2),(cubeX+1,cubeY+1),(cubeX+2,cubeY+2),(cubeX+1,cubeY+2),(cubeX+2,cubeY+1)) //
				case 1 => List((cubeX,cubeY+2),(cubeX,cubeY+3),(cubeX,cubeY+4),(cubeX+1,cubeY+2),(cubeX+1,cubeY+3),(cubeX+1,cubeY+4),(cubeX+2,cubeY+2),(cubeX+2,cubeY+3),(cubeX+2,cubeY+4)) //
				case 2 => List((cubeX,cubeY+4),(cubeX,cubeY+5),(cubeX,cubeY+6),(cubeX+1,cubeY+4),(cubeX+1,cubeY+5),(cubeX+1,cubeY+6),(cubeX+2,cubeY+4),(cubeX+2,cubeY+5),(cubeX+2,cubeY+6)) //
			}
			case 1 => cubeY match {
				case 0 => List((cubeX+2,cubeY),(cubeX+2,cubeY+1),(cubeX+2,cubeY+2),(cubeX+3,cubeY),(cubeX+3,cubeY+1),(cubeX+3,cubeY+2),(cubeX+4,cubeY),(cubeX+4,cubeY+1),(cubeX+4,cubeY+2))
				case 1 => List((cubeX+2,cubeY+2),(cubeX+2,cubeY+3),(cubeX+2,cubeY+4),(cubeX+3,cubeY+2),(cubeX+3,cubeY+3),(cubeX+3,cubeY+4),(cubeX+4,cubeY+2),(cubeX+4,cubeY+3),(cubeX+4,cubeY+4))
				case 2 => List((cubeX+2,cubeY+4),(cubeX+2,cubeY+5),(cubeX+2,cubeY+6),(cubeX+3,cubeY+4),(cubeX+3,cubeY+5),(cubeX+3,cubeY+6),(cubeX+4,cubeY+4),(cubeX+4,cubeY+5),(cubeX+4,cubeY+6))
			}
			case 2 =>  cubeY match {
				case 0 => List((cubeX+4,cubeY),(cubeX+4,cubeY+1),(cubeX+4,cubeY+2),(cubeX+5,cubeY),(cubeX+5,cubeY+1),(cubeX+5,cubeY+2),(cubeX+6,cubeY),(cubeX+6,cubeY+1),(cubeX+6,cubeY+2)) 
				case 1 => List((cubeX+4,cubeY+2),(cubeX+4,cubeY+3),(cubeX+4,cubeY+4),(cubeX+5,cubeY+2),(cubeX+5,cubeY+3),(cubeX+5,cubeY+4),(cubeX+6,cubeY+2),(cubeX+6,cubeY+3),(cubeX+6,cubeY+4))
				case 2 => List((cubeX+4,cubeY+4),(cubeX+4,cubeY+5),(cubeX+4,cubeY+6),(cubeX+5,cubeY+4),(cubeX+5,cubeY+5),(cubeX+5,cubeY+6),(cubeX+6,cubeY+4),(cubeX+6,cubeY+5),(cubeX+6,cubeY+6))
			}
		}
	}
}

// Top -left  corner  is (0,0).  Bottom -right  corner  is (8 ,8).  Feel  free to
//  change  the  fields  of this  class.
class  Board(val  available: Map[(Int , Int), List[Int ]])  extends  BoardLike[Board] {
	def  availableValuesAt(row: Int , col: Int): List[Int] = {
		//  Assumes  that a missing  value  means  all  values  are  available. Feel
		// free to  change  this.
		available.getOrElse ((row , col), 1.to(9). toList)
	}

	def  valueAt(row: Int , col: Int):  Option[Int] = {
		val values = available.getOrElse((row , col), 1.to(9). toList)
		if (values.length == 1){
			Some(values(0))
		} else {
			None
		}
	}

	def  isSolved ():  Boolean = {
		isSolvedHelper(0,0)
	}


	def isSolvedHelper(row: Int,col: Int):Boolean = {
		val value = valueAt(row,col)
		//println("row: " + row + "col:" + col + "" + "value: "+ value )
		

		if (value == None){
			false
		} else{
			if (row == 8 && col == 8){
				true  // game board is complete
			} else {
				if (col == 8){
					isSolvedHelper(row+1,0) // reset row to 0
				} else {
					isSolvedHelper(row,col+1) 
				}
			}
		}
	}


	def  isUnsolvable ():  Boolean = {
		isUnsolvedHelper(0,0)
	}



	def isUnsolvedHelper(row: Int,col: Int):Boolean = {
		val values = availableValuesAt(row,col)

		if (values == Nil){
			true
		} else{
			if (row == 8 && col == 8){
				false  // game board is complete
			} else {
				if (col == 8){
					isUnsolvedHelper(row+1,0) // reset row to 0
				} else {
					isUnsolvedHelper(row,col+1) 
				}
			}
		}
	}

	def  place(row: Int , col: Int , value: Int):  Board = {
		require(availableValuesAt(row , col). contains(value))

		val newB = new Board((available - ((row,col))) + ((row,col) -> List(value))) 
		val retB = new Board(removePeerss(value,Solution.peers(row,col),newB.available))

		retB
		
	}


	def removePeerss(toRemove: Int,lst: List[(Int , Int)],map: Map[(Int , Int), List[Int ]]): Map[(Int , Int), List[Int ]] = {
		
		if (lst.length < 1){
			map
		} else{
			val ((row, col) :: rest) = lst

			
			//val row
			//val col
			//val rest - the rest of peers list
			val currMap = map((row,col)) // the current maps list
			//print("map: " + lst.toString)

			if (currMap.length == 1){
				removePeerss(toRemove, rest, (map - ((row,col))) + ((row,col) -> currMap.filter(_ != toRemove)))
			} else {
				val newList = currMap.filter(_ != toRemove)
				if ((currMap.filter(_ != toRemove)).length == 1){
					//println("map: " + currMap.toString)
					//println("remove: " + toRemove)
					//println("new peers: " + peers(row,col).toString)
					val newRemove = currMap.filter(_ != toRemove)(0)

					val newMap = removePeerss(newRemove, Solution.peers(row,col), (map - ((row,col))) + ((row,col) -> (List[Int](newRemove))))
					removePeerss(toRemove, rest, newMap)	

				} else {
					removePeerss(toRemove, rest, (map - ((row,col))) + ((row,col) -> currMap.filter(_ != toRemove)))				
				}
			}
		}
	}



	// You  can  return  any  Iterable (e.g., Stream)
	def  nextStates ():  List[Board] = {

		if (isUnsolvable ()) {
			List() // if unsolvable return empty list
		}
		else {
			nextStatesHelper(0,0,Nil,available) // helper - iterates 0,0 - 8,8 by adding 1 value from each list at ever single position
		}
	}

	//iterates through the list of values to place each value in the list at the given coordinates returning a list of the boards produced 
	def placer(row:Int,col:Int,b: Board,vals: List[Int],lst: List[Board],valsIndex:Int): List[Board] = {
		if (valsIndex == 0){
			lst
		} else {
			val (num :: rest) = vals
			val newRet = lst ::: List(place(row,col,num))

			placer(row,col,b,rest,newRet,valsIndex-1)
		}
	}



	def nextStatesHelper(row: Int,col: Int,ret: List[Board],map: Map[(Int , Int), List[Int ]]) : List[Board] = {

		val values = availableValuesAt(row,col)

		//println("row: " + row + "col:" + col + "" + "values: "+ values )

		val newret = placer(row,col,new Board(map),values,ret,values.length)


		if (row == 8 && col == 8){
			val res = ret.sortWith((x, y) => x.available.toList.length < y.available.toList.length)
			//println(res)
			res
		} else {
			if (col == 8){
				nextStatesHelper(row+1,0,newret,available)
			}else{
				nextStatesHelper(row,col+1,newret,available) 
			}
		}
	}


	def solveHelper(index: Int,lst: List[Board]): Option[Board] = {
		val currBoard :: rest = lst
		val newStates = currBoard.nextStates()
		val newIndex = newStates.length

		if (currBoard.isSolved()){
			Some(currBoard)
		} else{
			if (index == 0){
				
				solveHelper(newIndex,rest)
			}else{
				if (currBoard.isUnsolvable()){	
					Some(currBoard)

				} else{
					solveHelper(newIndex-1,rest)
				}
			}
		}
	}


	def checkStates(states: List[Board]): Option[Board] = {
		val currBoard :: rest = states
		println("current Board"+currBoard+"isSolved: "+currBoard.isSolved())
		
		if (currBoard.isSolved()){
			Some(currBoard)
		} else {

			if (rest.length != 0){
				checkStates(rest)	
			} else {
				Some(currBoard)
			}
		}

	}

	def  solve ():  Option[Board] = {
		if (isSolved()){
			Some(new Board(available))
		} else {
			checkStates(nextStates())
		}
	}
}