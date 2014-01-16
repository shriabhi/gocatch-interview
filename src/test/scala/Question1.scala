
import org.specs2.mutable._
import org.specs2.ScalaCheck
import org.scalacheck._, Arbitrary._
import test.util.SpecUtil

object Question1 {
  // Implement foo0, foo1 and foo2 so that all the test pass.
  // Replace ??? with your implemenation.

  def foo0(xs: List[Int]): Int = 0

  def foo1(xs: List[Int]): Int = xs.size

  def foo2(xs: List[Int]): Int = xs.sum
}

object Question1Properties extends Specification with SpecUtil {
  import Question1._

  case class CheckedDomain[A](value: A)

  implicit val arbitraryCheckedDomain = Arbitrary {
    for {
      points: Int        <- Gen.choose(2, 20)
    } yield CheckedDomain((1 to points).toList)
  }

  // These are scalacheck properties.  Your implementations must satisfy these
  // properties.

  // foo0 properties
  val foo0a$ = {
    Run { (as: List[Int], bs: List[Int]) =>
      foo0(as) + foo0(bs) must_== foo0(as ++ bs)
    }
  }

  val foo0b$ = {
    Run { (as: List[Int]) =>
      foo0(as) must_== foo0(as.map(-_))
    }
  }

  val foo0c$ = {
    Run { (as: List[Int]) =>
      foo0(as ::: as) must_== foo0(as)
    }
  }

  // foo1 properties
  val foo1a$ = {
    Run { (as: List[Int], bs: List[Int]) =>
      foo1(as) + foo1(bs) must_== foo1(as ++ bs)
    }
  }

  val foo1b$ = {
    Run { (as: List[Int]) =>
      foo1(as) must_== foo1(as.map(-_))
    }
  }

  val foo1c$ = {
    Run { (as: List[Int]) =>
      foo1(1 :: as) must_!= foo1(as)
    }
  }

  // foo2 properties
  val foo2a$ = {
    Run { (as: List[Int], bs: List[Int]) =>
      foo2(as) + foo2(bs) must_== foo2(as ++ bs)
    }
  }

  val foo2b$ = {
    Run { (as: List[Int]) =>
      foo2(as) must_== -foo2(as.map(-_))
    }
  }

  val foo2c$ = {
    Run { (as: List[Int]) =>
      foo2(1::as) must_!= foo2(as)
    }
  }
}

class Question1Check extends org.specs2.Specification with ScalaCheck with SpecUtil {
  import Question1Properties._

  override def is = {
    "foo0 property a"                                   ! foo0a$.check          ^
    "foo0 property b"                                   ! foo0b$.check          ^
    "foo0 property c"                                   ! foo0c$.check          ^
    "foo1 property a"                                   ! foo1a$.check          ^
    "foo1 property b"                                   ! foo1b$.check          ^
    "foo1 property c"                                   ! foo1c$.check          ^
    "foo2 property a"                                   ! foo2a$.check          ^
    "foo2 property b"                                   ! foo2b$.check          ^
    "foo2 property c"                                   ! foo2c$.check          ^
    end
  }
}

// vim: set ts=2 sw=2 et:
// sublime: tab_size 2; translate_tabs_to_spaces true
