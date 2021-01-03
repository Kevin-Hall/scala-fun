


object  FunctionalDataStructures {

	case  class  Queue[A](front: List[A], back: List[A])

		def  append[A](lst1: List[A] , lst2: List[A]):  List[A] = lst1  match {
			case  Nil => lst2
			case  head :: tail => head :: append(tail , lst2)
		}

		def  enqueue[A](elt: A, q: Queue[A]):  Queue[A] = q match {
			case Queue(Nil,Nil) => Queue(List(elt), Nil)
			case Queue(head,Nil) => Queue(append(List(elt),head), Nil)
			case Queue(Nil,tail) => Queue(List(elt), tail)
			case Queue(head,tail) => Queue(append(List(elt),head), tail)
		}
		
		def  dequeue[A](q: Queue[A]):  Option [(A, Queue[A])] = q match {
			case Queue(Nil,Nil) => None
			case Queue(Nil,tail) => tail.reverse match {
										case Nil => None
										case h :: t => Option(h,Queue(t,Nil))
									}
			case Queue(head,Nil) => Option(head.last,Queue(head,Nil))
			case Queue(head,tail) => tail.reverse match {
										case Nil => Option(head.last,Queue(head,Nil))
										case h :: t => Option(h,Queue(head,t))
									}
		}
	
	sealed  trait  JoinList[A] {
		val  size: Int
	}

	case  class  Empty[A]()  extends  JoinList[A] {
		val  size = 0
	}

	case  class  Singleton[A](elt: A) extends  JoinList[A] {     //single element to be placed in a queue 
		val  size = 1
	}

	case  class  Join[A](lst1: JoinList[A], lst2: JoinList[A], size: Int) extends  JoinList[A]
	
		//returns max element 
		def  max[A](lst: JoinList[A], compare: (A, A) => Boolean ):  Option[A] = lst match {
			case Empty() => None
			case Singleton(x) => Some(x)
			case Join(Empty(),Empty(),_) => None
			case Join(Singleton(x),Empty(),_) => Some(x)
			case Join(Empty(),Singleton(x),_) => Some(x)
			case Join(Singleton(x),Singleton(y),_) => if (compare(x,y)) {Some(x)} else {Some(y)}
			case Join(Empty(),lst2,_) => max(lst2,compare)
			case Join(lst1,Empty(),_) => max(lst1,compare)
			case Join(lst1,lst2,_) => (max(lst1,compare), max(lst2,compare)) match {
										  case (Some(x), Some(y)) => if (compare(x,y)) {Option(x)} else {Option(y)}
										  case (Some(x), None) => Option(x)
										  case (None, Some(y)) => Option(y)
										  case (None, None) => None
										}
		}
	
		//returns first item
		def  first[A](lst: JoinList[A]):  Option[A] = lst match {
			case Empty() => None
			case Singleton(x) => Some(x)
			case Join(Empty(),Empty(),_) => None
			case Join(Empty(),Singleton(x),_) => Some(x)
			case Join(Singleton(x),Empty(),_) => Some(x)
			case Join(Singleton(x),Singleton(y),_) => Some(x)
			case Join(lst1,lst2,_) => first(lst1)

		}

		//returns last item 
		def  rest[A](lst: JoinList[A]):  Option[JoinList[A]] = lst match {
			case Empty() => None
			case Singleton(x) => Some(Empty())
			case Join(Empty(),Empty(),_) => None
			case Join(Empty(),Singleton(x),_) => Some(Empty())
			case Join(Singleton(x),Empty(),_) => Some(Empty())
			case Join(Singleton(x),Singleton(y),_) => Option(Join(Singleton(y),Empty(),1))
			case Join(lst1,lst2,_) => rest(lst2)
		}
		
		//helper
		def removeFirst[A](lst:JoinList[A]): JoinList[A] = lst match {
			case Empty() => Empty()
			case Singleton(x) => Empty()
			case Join(Empty(),Empty(),_) => Empty()
			case Join(Empty(),Singleton(x),_) => Empty()
			case Join(Singleton(x),Empty(),_) => Empty()
			case Join(Singleton(x),Singleton(y),_) => Join(Singleton(y),Empty(),1)
			case Join(lst1,lst2,_) => Join(removeFirst(lst1),lst2,lst1.size-1+lst2.size)
		}

		//returns the item at a given index
		def  nth[A](lst: JoinList[A], n: Int):  Option[A] = n match {
			case 0 => first(lst)
			case n => lst match {
				case Empty() => None
				case Singleton(x) => None
				case Join(Empty(),Empty(),_) => None
				case Join(Empty(),Singleton(x),_) => None
				case Join(Singleton(x),Empty(),_) => None
				case Join(Singleton(x),Singleton(y),_) => if (n == 1){Option(y)}else{None}
				case Join(Empty(),lst2,_) => nth(lst2,n)
				case Join(lst1,lst2,_) => nth(Join(removeFirst(lst1),lst2,lst1.size-1+lst2.size),n-1)
			}
		}

		//returns a joinlist after mapping each element 
		def  map[A,B](f: A => B, lst: JoinList[A]):  JoinList[B] = lst match {
			case Empty() => Empty()
			case Singleton(x) => Singleton(f(x))
			case Join(Empty(),Empty(),_) => Empty()
			case Join(Empty(),Singleton(x),_) => Singleton(f(x))
			case Join(Singleton(x),Empty(),_) => Singleton(f(x))
			case Join(Singleton(x),Singleton(y),_) => Join(Singleton(f(x)),Singleton(f(y)),2)
			case Join(lst1,lst2,_) => Join(map(f,lst1),map(f,lst2),lst1.size + lst2.size)
		}

		//returns the list of elements that satisfy pred
		def  filter[A](pred: A => Boolean , lst: JoinList[A]):  JoinList[A] = lst match {
			case Empty() => Empty()
			case Singleton(x) => if (pred(x)) {Singleton(x)} else {Empty()}
			case Join(Empty(),Empty(),_) => Empty()
			case Join(Empty(),Singleton(x),_) => if (pred(x)) {Singleton(x)} else {Empty()}
			case Join(Singleton(x),Empty(),_) => if (pred(x)) {Singleton(x)} else {Empty()}
			case Join(Singleton(x),Singleton(y),_) => if (pred(x)&&pred(y)) { Join(Singleton(x),Singleton(y),2)} 
														else if (pred(x)) {Singleton(x)} 
														else if (pred(y)) {Singleton(y)} 
														else {Empty()}       
			case Join(lst1,lst2,_) => Join(filter(pred,lst1),filter(pred,lst2),lst1.size + lst2.size) 
		}

}