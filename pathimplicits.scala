

object  PathImplicits{

import java.nio.file.Paths
import java.nio.file.Path
import java.nio.file.Files
import java.io.File


	implicit  class  PathsClass(pathStr: String) {
		def /(str: String) : Path = {
			Paths.get(pathStr, str)
		}
	}

	implicit  class  PathsClassTwo(path: Path) {
		def /(str: String) : Path = {
			Paths.get(path.toString, str)
		}

		def write(str: String) = {
			if (Files.notExists(path)){
				Files.write(path, str.getBytes())
			} else{
				Files.write(path,str.getBytes())
			}
		}

		def read() = {
			val content = Files.readAllBytes(path)
			val str = new String(content,"UTF-8")
			str
		}

		def append(str: String) = {
			if (Files.notExists(path)){
				Files.write(path, str.getBytes())
			} else{
				val bytes = Files.readAllBytes(path) 
				val allbytes = bytes ++ str.getBytes()
				Files.write(path,allbytes)
			}
		}
	}
}


object DateImplicits {

import scala.language.implicitConversions
import java.time.LocalDate
import java.time._

	implicit class Dates(day: Int) {

		def jan() = {LocalDate.of(2016, 1, day)}
		def feb() = {LocalDate.of(2016, 2, day)}
		def mar() = {LocalDate.of(2016, 3, day)}
		def apr() = {LocalDate.of(2016, 4, day)}
		def may() = {LocalDate.of(2016, 5, day)}
		def jun() = {LocalDate.of(2016, 6, day)}
		def jul() = {LocalDate.of(2016, 7, day)}
		def aug() = {LocalDate.of(2016, 8, day)}
		def sep() = {LocalDate.of(2016, 9, day)}
		def oct() = {LocalDate.of(2016, 10, day)}
		def nov() = {LocalDate.of(2016, 11, day)}
		def dec() = {LocalDate.of(2016, 12, day)}

		def jan(year: Int) = {LocalDate.of(year, 1, day)}
		def feb(year: Int) = {LocalDate.of(year, 2, day)}
		def mar(year: Int) = {LocalDate.of(year, 3, day)}
		def apr(year: Int) = {LocalDate.of(year, 4, day)}
		def may(year: Int) = {LocalDate.of(year, 5, day)}
		def jun(year: Int) = {LocalDate.of(year, 6, day)}
		def jul(year: Int) = {LocalDate.of(year, 7, day)}
		def aug(year: Int) = {LocalDate.of(year, 8, day)}
		def sep(year: Int) = {LocalDate.of(year, 9, day)}
		def oct(year: Int) = {LocalDate.of(year, 10, day)}
		def nov(year: Int) = {LocalDate.of(year, 11, day)}
		def dec(year: Int) = {LocalDate.of(year, 12, day)}
	}



	implicit class myInt(i: Int) {

			def days() = {i}
			def months() = {Month.of(i)}
			def years() = {Year.of(i)}
	}

	implicit class MoreDates(ld: LocalDate) {

		def +(days:Int) = {ld.plusDays(days)}
		def +(months: Month) = {ld.plusMonths(months.getValue())}
		def +(years: Year) = {ld.plusYears(years.getValue())}
	}
	



}
