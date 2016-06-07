package com.exercises.chapter

import fpinscala.datastructures.List._
import fpinscala.datastructures.{List => MyList, _}
import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Gen}

class threeSpec extends BaseSpec {
  private val myNilGen: Gen[MyList[Nothing]] = Gen.delay(Nil)

  implicit def myDoubleArbitrary = Arbitrary(Gen.chooseNum(-100.00, 100.00))
  implicit def myListArbitrary[T: Arbitrary]: Arbitrary[MyList[T]] = Arbitrary[MyList[T]](Gen.oneOf(myNilGen, myConsGen[T]))

  private def myConsGen[T: Arbitrary]: Gen[MyList[T]] = for {
    head <- Arbitrary.arbitrary[T]
    tail <- Gen.oneOf(myNilGen, myConsGen[T])
  } yield Cons(head, tail)

  "List Pattern Matching " should {
    "return 3 " in {
      val res = three.ListPatternMatching
      res should be(3)
    }
  }

  "List tail " should {
    "return a list without the first element of the original list" in {
      val property = forAll { (l: MyList[Int]) => {
        val res = tail(l)

        l match {
          case Cons(_, res) => true
          case Nil if res == Nil => true
          case _ => false
        }
      }
      }

      check(property)
    }
  }

  "List setHead " should {
    "return a list with the first element replaced by the passed argument" in {
      val property = forAll {
        (l: MyList[Int], i: Int) => {
          val res = setHead(l, i)
          tail(l).equals(tail(res))
        }
      }

      check(property)
    }
  }

  "List drop " should {
    "return a list without the Nth first elements" in {
      @annotation.tailrec
      def aux(ml: MyList[Int], a: Int): MyList[Int] = a match {
        case x if x <= 0 => ml
        case x => aux(tail(ml), x - 1)
      }

      val property = forAll {
        (l: MyList[Int], i: Int) => {
          drop(l, i) == aux(l, i)
        }
      }

      check(property)
    }
  }

  "List drop while" should {
    "return an empty List when receiving and empty list " in {
      val res = dropWhile(Nil, { _: Int => false })

      res shouldBe (Nil)
    }

    "return an empty list if all elements match the function passed " in {
      val property = forAll {
        (l: MyList[Int]) => {
          dropWhile(l, { _: Int => true }) == Nil
        }
      }

      check(property)
    }

    "return an the original list if no elements match the function passed " in {
      val property = forAll {
        (l: MyList[Int]) => {
          dropWhile(l, { _: Int => false }) == l
        }
      }

      check(property)
    }

    "return a list without the Nth first negative elements from the original list" in {
      @annotation.tailrec
      def aux(lst: MyList[Int]): MyList[Int] = lst match {
        case Cons(h, t) if h < 0 => aux(t)
        case _ => lst
      }

      val property = forAll {
        (l: MyList[Int]) => {
          val res = dropWhile(l, { a: Int => a < 0 })
          res == aux(l)
        }
      }

      check(property)
    }
  }

  "List length" should {
    @annotation.tailrec
    def aux(lst: MyList[Int], acum: Int) : Int = lst match {
      case Nil => acum
      case Cons(_, t) => aux(t, acum + 1)
    }

    "return 0 if the list is empty" in {
        MyList.length(Nil) should be (0)
    }

    "properly calculate the length of the list" in {
      val property = forAll {
        (l: MyList[Int]) => {
          MyList.length(l) == aux(l, 0)
        }
      }

      check(property)
    }
  }

  "sumFoldLeft" should {
    "return the same result as sum for the same input" in {
      check(forAll { (l: MyList[Int]) => sum(l) == sumFoldLeft(l)})
    }
  }

  "productFoldLeft" should {
    "return the same result as product for the same input" in {
      val prop = forAll { (l: MyList[Double]) =>
        val diff = product(l) - productFoldLeft(l)

        diff < 1E-12
      }

      check(prop)
    }
  }


}
