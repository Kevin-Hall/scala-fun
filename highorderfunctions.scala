
object  HOF {
	
	def map2[A,B,C](f: (A, B) => C, lst1: List[A], lst2: List[B]): List[C] = lst1 match {
		case Nil => Nil
		case h :: t => lst2 match {
			case x :: y => append(List(f(h,x)),map2(f,t,y))
			case Nil => Nil
		}

	}

	def zip[A,B](lst1: List[A], lst2: List[B]): List[(A, B)] = lst1 match{
		
		case h :: t => lst2 match{
			case Nil => Nil
			case x :: y => append(List((h,x)),zip(t,y))
		}
		case Nil => Nil
		case _ => Nil
	}


	//helper to combine two lists
	def  append[A](lst1: List[A] , lst2: List[A]):  List[A] = lst1  match {
		case  Nil => lst2
		case  head :: tail => head :: append(tail , lst2)
	}

	def flatten[A](lst: List[List[A]]): List[A] = lst match {
		//case headOne :: restOne => headOne :: removeElem(restOne)
		case head :: tail => append(head,flatten(tail))
		case Nil => Nil
	}

	def flatten3[A](lst: List[List[List[A]]]): List[A] = lst match{	
		case head :: tail :: rest => append(append(flatten(head),flatten(tail)),flatten3(rest))
		case Nil => Nil
		case _ => Nil
	}


	def buildList[A]( length: Int , f: Int => A): List[A] = { 
		if (length > 0){append(buildList(length-1,f),List(f(length-1)))} else {Nil}
	}

	def mapList[A, B](lst: List[A], f: A => List[B]): List[B] = lst match{
		case h :: t => append(f(h),mapList(t,f))
		case  Nil => Nil
	}

	def part[A](f: A => Boolean, lst: List[A], lst1: List[A], lst2: List[A]): (List[A],List[A]) = {
		lst match {
			case h :: t => if (f(h)) {part(f,t,lst1:+h,lst2)} else {part(f,t,lst1,lst2:+h)}
			case Nil => (lst1,lst2)
		}
	}

	def partition[A](f: A => Boolean , lst: List[A]): (List[A], List[A]) = {
		part(f,lst,List[A](),List[A]())
	}

	def mergeHelp[A]( lessThan: (A, A) => Boolean , alist1: List[A], alist2: List[A],toList: List[A]): List[A] = {
			(alist1, alist2) match {
				case (head :: tail,h :: t) => if (lessThan(h,head)) {mergeHelp(lessThan,tail,h :: t,toList:+head)} 
											else if (lessThan(head,h)) {mergeHelp(lessThan,head :: tail,t,toList:+h)} else {Nil}
				case (Nil,head :: tail) => mergeHelp(lessThan,Nil,tail,toList:+head)
				case (head :: tail,Nil) => mergeHelp(lessThan,tail,Nil,toList:+head)
				case (Nil,Nil) => toList
			}
			
	}
	

	def merge[A]( lessThan: (A, A) => Boolean , alist1: List[A], alist2: List[A]): List[A] = {
		mergeHelp(lessThan,alist1,alist2,List[A]())
		
	}

	def sort[A]( lessThan: (A, A) => Boolean , alist: List[A]): List[A] = {
			def splitter[A](f: A => Boolean, lis: List[A] , tolist1: List[A], toList2: List[A]): (List[A],List[A]) = {lis match {
				case h :: t => if (f(h)) {splitter(f,t,tolist1:+h,toList2)} 
								else if (!f(h)) {splitter(f,t,tolist1,toList2:+h)}
				case Nil => (tolist1,toList2)
			}
				merge(lessThan,tolist1,toList2)
			}
			
	}

}






















