package randomForest

import org.scalatest.FunSuite

import DTree._
import math._

class DTreeTests extends FunSuite {

  test("log2 basics") {
    assert(log2(1.0) == 0)
    assert(log2(2.0) == 1)
    assert(abs(log2(10.0) - 3.3219280948873626) < math.pow(10, -15))
  }
}
