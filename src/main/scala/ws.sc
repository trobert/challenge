
//isPrime(3)


def f(s:String) = "f"+s
def g(s:String) = "g"+s

val a = "a"
val s:Stream[String] = a #:: (s flatMap {e ⇒ f(e) #:: g(e) #:: Stream.empty})

s take 7 toList

val primes: Stream[Int] = 1 #:: 2 #:: (Stream.from(3,2) filter isPrime)
def isPrime(n: Int) = primes.tail.takeWhile(p ⇒ p*p <= n).forall(n % _ != 0)
primes.take(100).toList
