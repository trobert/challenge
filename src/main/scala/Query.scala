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

  object CSInts {
    def unapply(s: String) = Some(s.split(", ").map(_.toInt))
  }
  object CSV {
    def unapply(s: String) = Some(s.split(", "))
  }
  case class CSV(values: Seq[Any]) {
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
    case r"which of the following numbers are primes: (.*)${ CSInts(a) }"     ⇒ CSV(a filter isPrime)
    case r"which of the following numbers is the largest: (.*)${ CSInts(a) }" ⇒ a.max
    case r"which of the following numbers is both a square and a cube: (.*)${ CSInts(a) }" ⇒
      CSV(a.filter(n ⇒ isSquare(n) && isCube(n)))
    case ri"what is the (\d+)$n.. number in the Fibonacci sequence" ⇒ fib(n)
    case "who played James Bond in the film Dr No"                  ⇒ "Sean Connery"
    case "what colour is a banana"                                  ⇒ "yellow"
    case "who is the Prime Minister of Great Britain"               ⇒ "David Cameron"
    case "what currency did Spain use before the Euro"              ⇒ "peseta"
    case "which city is the Eiffel tower in"                        ⇒ "Paris"
    case "what is your name"                                        ⇒ "tito"
    case ri"what is (\d+)$a multiplied by (\d+)$b plus (\d+)$c"     ⇒ a * b + c
    case ri"what is (\d+)$a plus (\d+)$b multiplied by (\d+)$c"     ⇒ a + b * c
    case r"what is the english scrabble score of (.*)$a"            ⇒ (a map score).sum
    case r"""which of the following is an anagram of "(.*)$a": (.*)${ CSV(b) }""" ⇒
      CSV(b filter (_.sorted == a.sorted))
    case s ⇒
      println(s"Query: $s")
      "I don't know"
  }

  def isSquare(n: Int) = Math.sqrt(n).isWhole()
  def isCube(n: Int) = Math.cbrt(n).isWhole()
  def isPrime(n: Int) = (n > 0) && (2 until n forall (n % _ != 0))

  val fib: Stream[BigInt] = 0 #:: 1 #:: fib.zip(fib.tail).map(p => p._1 + p._2)
  val score = Map(
    //in nachtergaele order
    'a' -> 1, 'c' -> 3, 'b' -> 3, 'e' -> 1, 'd' -> 2, 'g' -> 2,
    'f' -> 4, 'i' -> 1, 'h' -> 4, 'k' -> 5, 'j' -> 8, 'm' -> 3,
    'l' -> 1, 'o' -> 1, 'n' -> 1, 'q' -> 10, 'p' -> 3, 's' -> 1,
    'r' -> 1, 'u' -> 1, 't' -> 1, 'w' -> 4, 'v' -> 4, 'y' -> 4,
    'x' -> 8, 'z' -> 10)

}
