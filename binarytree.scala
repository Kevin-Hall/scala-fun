
import  hw.generics._


sealed  trait  BinTree[A] extends ListLike[A , BinTree[A]]
case class Node[A](lhs: BinTree[A], value: A, rhs: BinTree[A]) extends BinTree[A] {
	
	def cons(head: A): BinTree[A] = lhs match {
		case  Leaf() => Node(Leaf(),head,Leaf())
		case  Node(lhs,value,rhs) => lhs match {
			case Leaf() => Node(Node(Leaf(),head,Leaf()),value,Leaf()) // adds "head" in a node on lhs
			case Node(lhs,value,rhs) => lhs.cons(head)
		}
	}

	def head(): Option[A] = lhs match {
		case  Leaf() => None
		case  Node(lhs,value,rhs) => lhs match {
			case Leaf() => Some(value)
			case Node(lhs,value,rhs) => lhs.head()
		}
	}

	def isEmpty(): Boolean = lhs match {
		case  Leaf() => true
		case Node(lhs,value,rhs) => false
	}

	def tail(): Option[BinTree[A]] = rhs match {
		case  Leaf() => None
		case  Node(lhs,value,rhs) => rhs match {
			case Leaf() => Some(Node(lhs,value,rhs))
			case Node(lhs,value,rhs) => rhs.tail()
		}
	}

}
case class Leaf[A]() extends BinTree[A] {

	def cons(head: A): BinTree[A] = {
		Node(Leaf(),head,Leaf())
	}

	def head(): Option[A] = {
		None
	}

	def isEmpty(): Boolean = {
		true
	}

	def tail(): Option[BinTree[A]] = {
		None
	}
}

object  ListFunctions {
	def  filter[A, C <: ListLike[A, C]](f: A => Boolean, alist: C): C = {
		if (alist.isEmpty()) {alist}
		else{
			if (f(alist.head().get)){filter(f,alist.cons(alist.head().get))}else{filter(f,alist.tail().get)}
		}
	}

	def  append[A, C <: ListLike[A, C]](alist1: C , alist2: C): C = {
		if (alist1.isEmpty()) {alist2}
		else{
			append[A, C](alist1.cons(alist1.head().get),append[A, C](alist1.tail().get,alist2))
		}
	}

	def  sort[A  <: Ordered[A], C  <: ListLike[A, C]]( alist: C): C = {
		if (alist.isEmpty()){alist}
		else{
			 insert(alist.head().get,sort[A, C](alist.tail().get))
			}
		}
	

	def  insert[A  <: Ordered[A], C  <: ListLike[A, C]](x: A, alist: C):  C = {
		if (alist.isEmpty()){alist}
		else {if (x.compare(alist.head().get) == EQ || x.compare(alist.head().get) == LT) {alist.cons(x)} 
								else { append[A, C](alist.cons(alist.head().get),insert(x, alist.tail().get))}
		}
	}

}

class  C1 extends T2 [Int,Int,String,String] with T3[Int,Int,Int,String,String,String,Int]{
	// Do not  change  the  class  body. Simply  extend T1 , T2, and/or T3.
	def f(a: Int , b: Int): Int = 0
	def g(c: String ):  String = ""
	def h(d: String ): Int = 0
}

class  C2 extends T1[Int,Int] with T2[Int,Int,Int,Int] with T3[Int,Int,Int,Int,Int,Int,Int] {
	// Do not  change  the  class  body. Simply  extend T1 , T2, and/or T3.
	def f(a: Int , b: Int): Int = 0
	def g(c: Int):   Int = 0
	def h(d: Int): Int = 0
}

class  C3[A](x: A) extends T3[Int,A,Int,A,String,String,A]{
	// Do not  change  the  class  body. Simply  extend T1 , T2, and/or T3.
	def f(a: Int , b: A): Int = 0
	def g(c: A):  String = ""
	def h(d: String): A = x
}

class  C4[A](x: Int , y: C4[A]) extends T1[Int,C4[A]] with T3[Int,C4[A],C4[A],Int,C4[A],C4[A],Int]{
	// Do not  change  the  class  body. Simply  extend T1 , T2, and/or T3.
	def f(a: Int , b: C4[A]): C4[A] = b
	def g(c: Int): C4[A] = y
	def h(d: C4[A]):  Int = x
}