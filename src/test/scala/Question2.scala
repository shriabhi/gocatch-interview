

import org.specs2.mutable._
import org.specs2.ScalaCheck
import org.scalacheck._, Arbitrary._
import scala.collection.immutable.HashMap
import test.util.SpecUtil

object Question2 {
  val mc = new java.math.MathContext(1024)
  val zero = BigDecimal(0, mc)
  val one = BigDecimal(1, mc)


  var map = new HashMap[BigInt,BigInt]

  def bd(num:Int):BigInt = BigInt(num)

  def factorialBD(a:BigInt): BigInt = {
    def fac(n:BigInt):BigInt = {
      map.get(n) match {
        case Some(num) => {
          return num
        }
        case None => {
            if ( n == 1)
              bd(1)
            else{
              val result =n * fac(n-1)
              map += (n -> result)
              result
            }
          }
      }
    }
    fac(a)
  }

  def calculateTreeCombination(nodesNumber : Int) : BigDecimal = {
    val decimal: BigInt = (factorialBD(bd(2 * nodesNumber)) / (factorialBD(bd(nodesNumber + 1)) * factorialBD(bd(nodesNumber))))
    BigDecimal.apply(decimal,mc)
  }
  val bstCount: Int => BigDecimal = calculateTreeCombination
}

object Question2Properties extends Specification with SpecUtil {
  import Question2._

  case class CheckedDomain[A](value: A)

  // These are scalacheck properties.  Your implementations must satisfy these
  // properties.

  val bstCountSmall$ = {
    implicit val arbitraryCheckedDomain = Arbitrary {
      Gen.choose(1, 10).map(CheckedDomain(_))
    }

    Run { (n: CheckedDomain[Int]) =>
      bstCount(n.value) must_== Solution2.bstCount(n.value)
    }
  }

  val bstCountMedium$ = {
    implicit val arbitraryCheckedDomain = Arbitrary {
      Gen.choose(1, 100).map(CheckedDomain(_))
    }

    Run { (n: CheckedDomain[Int]) =>
      bstCount(n.value) must_== Solution2.bstCount(n.value)
    }
  }

  val bstCountLarge$ = {
    implicit val arbitraryCheckedDomain = Arbitrary {
      Gen.choose(1, 1000).map(CheckedDomain(_))
    }

    Run { (n: CheckedDomain[Int]) =>
      bstCount(n.value) must_== Solution2.bstCount(n.value)
    }
  }
}

class Question2Check extends org.specs2.Specification with ScalaCheck with SpecUtil {
  import Question2Properties._

  override def is = {
    "bstCount works for small values of n"             ! bstCountSmall$.check   ^
    "bstCount works for medium values of n"            ! bstCountMedium$.check  ^
    "bstCount works for large values of n"             ! bstCountLarge$.check   ^
    end
  }
}

// vim: set ts=2 sw=2 et:
// sublime: tab_size 2; translate_tabs_to_spaces true
