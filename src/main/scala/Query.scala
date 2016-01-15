import scala.util.matching.Regex

/**
 * Created with IntelliJ IDEA.
 * User: a203673
 * Date: 11/03/15
 * Time: 13:22
 */
object Query {

  implicit class RegexContext(sc: StringContext) {
    val r = new Regex(sc.parts.mkString, sc.parts.tail.map(_ ⇒ "x"): _*)
    object ri {
      def unapplySeq(s: CharSequence) = r.unapplySeq(s) map (l ⇒ l map (s ⇒ s.toInt))
    }
  }

  object CSV {
    def unapply(s: String) = Some(s.split(", ").toSeq.map(_.toInt))
  }
  case class CSV(values: Seq[Int]) {
    override def toString = values.mkString(", ")
  }

  def answerQuery(q: String) = q match {
    case r"\w*: (.*)$qry" ⇒
      println(qry)
      val answer = query(qry).toString
      println(answer)
      answer
    case _ ⇒ "???"
  }

  def query(q: String) = q match {
    case ri"what is (\d+)$a plus (\d+)$b"                                  ⇒ a + b
    case ri"what is (\d+)$a minus (\d+)$b"                                 ⇒ a - b
    case ri"what is (\d+)$a multiplied by (\d+)$b"                         ⇒ a * b
    case ri"what is (\d+)$a plus (\d+)$b plus (\d+)$c"                     ⇒ a + b + c
    case ri"what is (\d+)$a to the power of (\d+)$b"                       ⇒ Math.pow(a, b).toLong
    case r"which of the following numbers are primes: (.*)${ CSV(a) }"     ⇒ CSV(a filter isPrime)
    case r"which of the following numbers is the largest: (.*)${ CSV(a) }" ⇒ a.max
    case r"which of the following numbers is both a square and a cube: (.*)${ CSV(a) }" ⇒
      CSV(a.filter(n ⇒ isSquare(n) && isCube(n)))
    case ri"what is the (\d+)$n.. number in the Fibonacci sequence" ⇒ fib(n)
    case "who played James Bond in the film Dr No"                  ⇒ "Sean Connery"
    case "what colour is a banana"                                  ⇒ "yellow"
    case "who is the Prime Minister of Great Britain"               ⇒ "David Cameron"
    case "what currency did Spain use before the Euro"              ⇒ "peseta"
    case "which city is the Eiffel tower in"                        ⇒ "Paris"
    case "what is your name"                                        ⇒ "tito"
    case ri"what is (\d+)$a multiplied by (\d+)$b plus (\d+)$c"     => a * b + c
    case ri"what is (\d+)$a plus (\d+)$b multiplied by (\d+)$c"     => a + b * c
    case r"what is the english scrabble score of (.*)$a"            ⇒ scrabble_score(a)
    case r"""which of the following is an anagram of "(.*)$a": (.*)$b""" => (b.split(", ").filter(isAnagram(a, _))).mkString(", ")
    case s ⇒
      println(s"Query: $s")
      "I don't know"
  }

  def isSquare(n: Int) = Math.sqrt(n).isWhole()
  def isCube(n: Int) = Math.cbrt(n).isWhole()
  def isPrime(n: Int) = (n > 0) && ((2 to n - 1) forall (n % _ != 0))
  def isAnagram(a: String, b: String): Boolean = inAlphabeticalOrder(a).equals(inAlphabeticalOrder(b))

  def inAlphabeticalOrder(a: String) = a.sortWith((c1, c2) => c1 < c2)

  val fib: Stream[BigInt] = 0 #:: 1 #:: fib.zip(fib.tail).map(p => p._1 + p._2)
  val score: Map[String, Int] = Map("a" -> 1, "c" -> 3, "b" -> 3, "e" -> 1, "d" -> 2, "g" -> 2,
    "f" -> 4, "i" -> 1, "h" -> 4, "k" -> 5, "j" -> 8, "m" -> 3,
    "l" -> 1, "o" -> 1, "n" -> 1, "q" -> 10, "p" -> 3, "s" -> 1,
    "r" -> 1, "u" -> 1, "t" -> 1, "w" -> 4, "v" -> 4, "y" -> 4,
    "x" -> 8, "z" -> 10)

  def scrabble_score(word: String): Int = word.foldLeft(0) { case (acc, c) => acc + score.getOrElse(c.toString(), 0) }

}
