package com.exercises.chapter

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop._
import fpinscala.datastructures.{List => MyList, _}
import fpinscala.datastructures.List._

class threeSpec extends BaseSpec {
//  implicit def intArbitrary[T:Arbitrary] : Arbitrary[Int] = Arbitrary[Int](Gen.chooseNum(0, Int.MaxValue))
  implicit def myListArbitrary[T: Arbitrary]: Arbitrary[MyList[T]] = Arbitrary[MyList[T]](Gen.oneOf(myNilGen, myConsGen[T]))

  private def myConsGen[T: Arbitrary]: Gen[MyList[T]] = for {
    head <- Arbitrary.arbitrary[T]
    tail <- Gen.oneOf(myNilGen, myConsGen[T])
  } yield Cons(head, tail)

  private val myNilGen: Gen[MyList[Nothing]] = Gen.delay(Nil)

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

          val t = tail(l)

          res match {
            case Cons(i , t) => true
            case _ => false
          }
        }
      }

      check(property)
    }
  }

  "List drop " should {
    "return a list without the Nth first elements" in {
      def aux (ml: MyList[Int], a: Int) : MyList[Int] = a match {
        case x if x <= 0 => ml
        case x => aux (tail(ml), x - 1)
      }

      val property = forAll/*(myConsGen[Int], Gen.posNum[Int])*/ {
        (l: MyList[Int], i: Int) => {
          drop(l, i) == aux(l, i)
        }
      }

      check(property)
    }
  }

}
