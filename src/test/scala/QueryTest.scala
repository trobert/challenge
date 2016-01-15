import Query.{ CSV, query }
import org.scalatest.{ ShouldMatchers, FunSuite }

/**
 * Created with IntelliJ IDEA.
 * User: a203673
 * Date: 11/03/15
 * Time: 13:36
 */
class QueryTest extends FunSuite with ShouldMatchers{

  query("what is 6 multiplied by 4") should be(24)
  query("which of the following numbers are primes: 0, 3, 605, 11") should be(CSV(Seq(3,11)))

}
