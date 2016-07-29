package decisionTree

import org.scalatest.FunSuite

import DTree._
import math._

class DTreeTests extends FunSuite {

  test("log2 basics") {
    assert(log2(1.0) == 0)
    assert(log2(2.0) == 1)
    assert(abs(log2(10.0) - 3.3219280948873626) < math.pow(10, -15))
  }

  test("entropy") {
    assert(entropy(Vector(1,1,1)) == 0.0)
    assert(entropy(Vector(0,1,0,1)) == 1.0)
  }

  test("info gain") {
    assert(infoGain(Vector(0,1,0,1), Vector(0,0), Vector(1,1)) == 1.0)
  }

  test("best split for a feature") {
    assert(bestSplitFeature(Vector(Vector(0,0), Vector(1,1), Vector(2,1))) == (0, 0.9182958340544896))
  }

  test("get split") {
    assert(getSplit(Vector(Vector(0,1,0), Vector(1,0,1), Vector(2,1,1))) == (0, 0))
    assert(getSplit(Vector(Vector(1,0,0), Vector(0,1,1), Vector(1,2,1))) == (1, 0))
  }
}
