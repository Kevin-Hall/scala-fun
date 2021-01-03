
import  edu.umass.cs.CSV



object Dates  {

val expectCSV = CSV.fromFile("cdc-life-expectancy.csv")
val birthsCSV = CSV.fromFile("ssa-births.CSV")
//val birthsTinyCSV = CSV.fromFile("ssa-births copy.CSV")


	def yearIs(data: List[List[String]], n: Int): List[List[String ]] = {
			data.filter((v: List[String]) => (v(0).toInt == n))
	}

	def yearGT(data: List[List[String]], bound: Int): List[List[String ]] = {
		data.filter((v: List[String]) => (v(0).toInt > bound))
	}

	def yearLT(data: List[List[String]], bound: Int): List[List[String ]] = {
		data.filter((v:List[String]) => (v(0).toInt < bound))
	}

	def onlyName(data: List[List[String]], name: String ): List[List[String ]] = {
		data.filter((v:List[String]) => (v(1).equals(name)))
	}

	def mostPopular(data: List[List[String]]): (String , Int) = {
		val mapped = data.map((v:List[String]) => (v(1),0))
		val mappedNums = mapped.map{case(x:String,y:Int) => (x,count(onlyName(data,x)))}
		Console.println("list" + mappedNums)

		mappedNums.maxBy(_._2)


	}

	def count(data: List[List[String]]): Int = {
		val mapped = data.map((v:List[String]) => v(3).toInt)
		mapped.sum
	}

	def countGirlsAndBoys(data: List[List[String ]]): (Int , Int) = {
		val boys = data.filter((b: List[String]) => b(2).equals("M"))
		val girls = data.filter((g: List[String]) => g(2).equals("F"))
		(count(girls),count(boys))
	}

	def genderNeutralNames(data: List[List[String ]]): Set[String] = {
		val mapped = data.map ((v:List[String]) => (v(1),v(2)))
		val filteredMen = mapped.filter { case (x,y) => (y.equals("M"))}
		val mappedMen = filteredMen.map{case(v:String,x:String) => v}
		
		val filteredWomen = mapped.filter { case (x,y) => (y.equals("F"))}
		val mappedWomen = filteredWomen.map{case(v:String,x:String) => v}

		val menSet = mappedMen.toSet
		val womenSet = mappedWomen.toSet

		menSet.intersect(womenSet)
	}

	def  append[A](lst1: List[A] , lst2: List[A]):  List[A] = lst1  match {
		case  Nil => lst2
		case  head :: tail => head :: append(tail , lst2)
	}

	def flatten[A](lst: List[List[A]]): List[A] = lst match {
		case head :: tail => append(head,flatten(tail))
		case Nil => Nil
	}

	def expectedAlive(gender: String , birthYear: Int , currentYear: Int): Boolean = {
		val age = currentYear - birthYear
		//Console.println("age" + age)

		val lis = yearIs(expectCSV,birthYear)
		//Console.println("list" + lis)
		
		val expectedList = flatten(lis)
		//Console.println("flattened:" + expectedList)

		val mensExpected = expectedList(1).toInt
		//Console.println(mensExpected)
		val womensExpected = expectedList(2).toInt
		//Console.println(womensExpected)

		//Console.println(age<mensExpected)
		if (gender == "M") (age <= mensExpected)
		else  (age <= womensExpected)
	}

	def estimatePopulation(data: List[List[String]], year: Int): Int = {

		val boys = data.filter((b: List[String]) => b(2).equals("M"))
		val girls = data.filter((g: List[String]) => g(2).equals("F"))

		val girlsAlive = girls.filter((gir: List[String]) => (expectedAlive("F",gir(0).toInt,year)))
		val boysAlive = boys.filter((boy: List[String]) => (expectedAlive("M",boy(0).toInt,year)))


		(count(girlsAlive)+count(boysAlive))

	}

}






















