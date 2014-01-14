package test.util

import org.specs2._
import org.specs2.execute.Failure
import org.specs2.execute.FailureException
import org.specs2.matcher.MatchResult
import org.scalacheck._, Arbitrary._

trait SpecUtil extends SpecificationLike with ScalaCheck {
  var traceEnabled = false

  def trace(text: String): Unit = if (traceEnabled) println(text)

  def setTrace[T](value: Boolean)(f: => T): T = {
    val oldTraceEnabled = traceEnabled
    try {
      traceEnabled = value
      f
    } finally {
      traceEnabled = oldTraceEnabled
    }
  }

  case class Run[T, R](f: T => R)(implicit a: Arbitrary[T], s: Shrink[T]) extends (T => R) {
    override def apply(value: T): R = f(value)
    def check(implicit toProp: (=> R) => Prop): Prop = check1(f)
  }

  object Run {
    def apply[A: Arbitrary, B: Arbitrary, R](
        f: (A, B) => R): Run[(A, B), R] = Run { arbitrary: (A, B) =>
      f(arbitrary._1, arbitrary._2)
    }

    def apply[A: Arbitrary, B: Arbitrary, C: Arbitrary, R](
        f: (A, B, C) => R): Run[(A, B, C), R] = Run { arbitrary: (A, B, C) =>
      f(arbitrary._1, arbitrary._2, arbitrary._3)
    }

    def apply[A: Arbitrary, B: Arbitrary, C: Arbitrary, D: Arbitrary, R](
        f: (A, B, C, D) => R): Run[(A, B, C, D), R] = Run { arbitrary: (A, B, C, D) =>
      f(arbitrary._1, arbitrary._2, arbitrary._3, arbitrary._4)
    }

    def apply[A: Arbitrary, B: Arbitrary, C: Arbitrary, D: Arbitrary, E: Arbitrary, R](
        f: (A, B, C, D, E) => R): Run[(A, B, C, D, E), R] = Run { arbitrary: (A, B, C, D, E) =>
      f(arbitrary._1, arbitrary._2, arbitrary._3, arbitrary._4, arbitrary._5)
    }
  }

  def prop_[T, R](result: T => R)(implicit toProp: (=>R) => Prop, a: Arbitrary[T], s: Shrink[T]): Prop = check1(result)

  def verify[T](f: => Boolean): Boolean = f

  def pass = 1 must_== 1

  def fail(m: String): Nothing = {
    val realStackTrace = Thread.currentThread.getStackTrace.toList.drop(2)
    val usingStackTrace = realStackTrace.reverse.takeWhile { frame =>
      frame.getClassName != "test.accounts.SignupPassengerSpec$" &&
        frame.getMethodName != "fail"
    }.reverse
    val f = Failure(m, stackTrace = usingStackTrace)
    throw new FailureException(f)
  }

  type Tagged[U] = { type Tag = U }

  type @@[T, U] = T with Tagged[U]

  class Tagger[U] {
    def apply[T](t : T) : T @@ U = t.asInstanceOf[T @@ U]
  }

  def tag[U] = new Tagger[U]

  // Manual specialization needed here ... specializing apply above doesn't help
  def tag[U](i : Int) : Int @@ U = i.asInstanceOf[Int @@ U]
  def tag[U](l : Long) : Long @@ U = l.asInstanceOf[Long @@ U]
  def tag[U](d : Double) : Double @@ U = d.asInstanceOf[Double @@ U]
  def fetch[A](id: Int @@ A): A = null.asInstanceOf[A]
  def tag[U](arr: Array[Int]):Array[Int @@ U] = arr.asInstanceOf[Array[Int @@ U]]
}
