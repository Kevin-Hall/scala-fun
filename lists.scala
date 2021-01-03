
object  Lists {
	
	val  oddNumbers = 1 :: 3 :: 5 :: Nil


	def sumDouble (lis: List[Int]): Int = lis match {
		case Nil => 0
		case x :: y => 2 * x + sumDouble(y)
		//case x => sum = sum+x
	}

	def removeZeroes (lis: List[Int]): List[Int] = lis match {
		case Nil => Nil
		case 0 :: y => removeZeroes(y)
		case x :: y => x::removeZeroes(y)
	}
	
	def countEvens (lis: List[Int]): Int = lis match {
		case Nil => 0
		case x :: y => if (x % 2 == 0) {1 + countEvens(y)} else {countEvens(y)}
	}	

	def removeAlternating (lis: List[String]): List[String] = lis match {
		case Nil => Nil
		case x :: y :: z => x :: removeAlternating(z)
		case _ => lis
		//case x :: y => x::removeZeroes(y)
	}	


	def isAscending (lis: List[Int]): Boolean = lis match {
		case Nil => false
		case x :: y :: z => if (x <= y) {isAscending(y::z)} else {false}
		case _ => true
		//case x :: y => x::removeZeroes(y)
	}

	def addSub (lis: List[Int]): Int = lis match {
		case Nil => 0
		//case x :: y => 2 * x + sumDouble(y)
		//case x :: y :: z => if (x % 2 == 0) {x + countEvens(y)} else {countEvens(y)}
		case x :: y :: z :: a => x - y + z - addSub(a) 
		case x :: y :: z => x - y + addSub(z) 
		case x :: y => x - addSub(y)
		//case x :: y => 
		case _ => 0 
		//case x => sum = sum+x
	}

	def alternate(lisOne: List[Int], lisTwo: List[Int]) : List[Int] = lisOne match{
		case a :: b => lisTwo match{
			case x :: y =>  List(a,x) ++ alternate(b,y)
			case Nil => Nil
		}
		case Nil => Nil
		//case (a :: b, x :: y) => List(a,x) :: alternate(b,y)
	}

	def fromTo(num1: Int, num2: Int) : List[Int] = {
			if (num1 < num2) { 
				num1 :: fromTo(num1 + 1,num2)
			} else {Nil}

		}

	def insertOrdered(n: Int , lst: List[Int]): List[Int] = lst match{
		case Nil => Nil
		case x :: y :: z :: a => if (n > x && n < y) {x :: n :: y :: z :: insertOrdered(n,a)} else if (n > y && n < z){x :: y :: n :: z :: insertOrdered(n,a)} else {x :: y :: z :: insertOrdered(n,a)}
		case x :: y :: z => if (n > x && n < y) {x :: n :: y :: insertOrdered(n,z)} else {x :: y :: insertOrdered(n,z)}
		case x :: y => if (x > n) {n :: x :: y} else {x :: n :: y}
		case _ => Nil
		
	}


	def returnNext(list: List[Int]) : Int = list match {
		case Nil => 0
		case x :: y => x
	}

	def sort(list: List[Int]): List[Int] = list match {
    	case Nil => Nil
    	case x :: y => def sorter (x: Int, lis:List[Int]) :List[Int] = lis match{
           					case Nil => List(x)
           					case a :: b => if(x <= a){x :: a :: b} else {a :: sorter(x,b)}
       }
       sorter(x,sort(y))
 	}

}