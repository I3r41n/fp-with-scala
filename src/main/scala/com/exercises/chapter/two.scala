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
}
