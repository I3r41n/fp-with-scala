package com.exercises.chapter

/**
  * Created by bsantos on 18/05/16.
  */
class threeSpec extends BaseSpec {

  "List Pattern Matching " should {
    "return 3 " in {
      val res = three.ListPatternMatching
      res should be(3)
    }
  }

}
