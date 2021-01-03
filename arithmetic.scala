import  hw.parsing._
import  scala.util.parsing.combinator._
import  scala.math._

object  ArithEval  extends  ArithEvalLike {
	def  eval(e: Expr):  Double = e match {
		case Div(e1, e2) => eval(e1) / eval(e2)
		case Sub(e1, e2) => eval(e1) - eval(e2)
		case Exponent(e1, e2) => Math.pow(eval(e1),eval(e2))
    	case Add(e1, e2) => eval(e1) + eval(e2)
    	case Mul(e1, e2) => eval(e1) * eval(e2)
		case Num(e) => e
	}
}

object  ArithParser  extends  ArithParserLike {
	//  number: PackratParser[Double] is  defined  in  ArithParserLike
	lazy  val  atom: PackratParser[Expr] = number ^^ {case x => Num(x)} | (expr)
	lazy  val  exponent: PackratParser[Expr] = exponent ~ "^" ~ atom ^^ { case x ~ _ ~ y => Exponent(x, y) } | atom
	lazy  val  mul: PackratParser[Expr] = mul ~ "*" ~ exponent ^^ {case x ~ _ ~ y => Mul(x, y)}| mul ~ "/" ~ exponent ^^ {case x ~ _ ~ y => Div(x, y)} | exponent 
	lazy  val  add: PackratParser[Expr] = add ~ "+" ~ mul ^^ { case x ~ _ ~ y => Add(x, y) } | add ~ "-" ~ mul ^^ { case x ~ _ ~ y => Sub(x, y) } | mul
	lazy  val  expr: PackratParser[Expr] = add
}

object  ArithPrinter  extends  ArithPrinterLike {
	def  print(e: Expr):  String = e match {
		case Div(e1, e2) => s"(($e1)/($e2))"
		case Sub(e1, e2) => s"(($e1)-($e2))"
		case Exponent(e1, e2) => s"(($e1)^($e2))"
    	case Add(e1, e2) => s"(($e1)+($e2))"
    	case Mul(e1, e2) => s"(($e1)*($e2))"
		case Num(e) => s"($e)"
	}

}