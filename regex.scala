import  scala.util.matching.Regex

object  Regexes  extends  hw.regex.RegexLike {
	def  notAlphanumeric: Regex = new Regex("""([^a-zA-Z\d])+""")
	def  time: Regex = new Regex("(0[0-9]{1}|1[0-9]{1}|2[0-3]{1})(:)((0|1|2|3|4|5){1}[0-9]{1})")
	def  phone: Regex = new Regex("""\(([\d]{3})\)(\s)([\d]{3})(-)([\d]{4})""")
	def  zip: Regex = new Regex("""([\d]{5})((-)([\d]{4}))?""")
	def  comment: Regex = new Regex("""/\*([^*]|[\r\n])*\*/""")
	def  numberPhrase: Regex = new Regex("(twenty|thirty|fourty|fifty|sixty|seventy|eighty|ninety)(-)?(one|two|three|four|five|six|seven|eight|nine)?")
	def  roman: Regex = new Regex("(?=[XVI])(X?X?X?)(I[VX]|V?I?I?I?)")
	def  date: Regex = new Regex("""((\d\d([02468][1235679]|[13579][01345789]))-(((0[13578]|1[02])-(0[1-9]|[12]\d|3[01]))|((0[469]|11)-(0[1-9]|[12]\d|30)|02-(0[1-9]|1\d|2[0-8]))))|((\d\d([02468][048]|[13579][26]))-(((0[13578]|1[02])-(0[1-9]|[12]\d|3[01]))|((0[469]|11)-(0[1-9]|[12]\d|30)|02-(0[1-9]|[12]\d))))""")
	def  evenParity: Regex = new Regex("(([02468]*[13579]){2})*[02468]*")
}