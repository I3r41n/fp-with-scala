package com.exercises.chapter

import scala.annotation.tailrec

/**
  * Created by bsantos on 21/04/16.
  */
object two {
  // Exercise 1: Write a function to compute the nth fibonacci number

  def fibonacci(n: Int): Int = {

    @tailrec
    def aux (i: Int, previousFib: Int, currentFib: Int): Int = i match {
      case 0 => currentFib
      case i  if i > 0 => aux(i-1, currentFib, previousFib + currentFib)
      case _ =>  throw new RuntimeException
    }

    aux(n , 0 , 1)
  }

  // Exercise 2: Implement isSorted, which checks whether an Array[A] is sorted according to a
  // given comparison function
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {

    def aux(array: Array[A]): Boolean = array match {
      case Array() | Array(_) => true
      case _  if !ordered(array.head, array.tail.head) => false
      case _ => aux(array.tail)
    }

    aux(as)
  }
}
