package decisionTree

import org.scalatest.FunSuite

import RandomForest._
import util.Random

class randomForestTests extends FunSuite {

  test("sub sample of matrix rows") {
    Random.setSeed(42)
    val d = Vector(Vector(1,2,3,4), Vector(5,6,7,8), Vector(9,10,11,12))
    assert(subSample(d, 2) == Vector(Vector(1,2,3,4), Vector(5,6,7,8)))
  }

  test("mean") {
    assert(mean(List(0.0,0.0,0.0)) == 0.0)
    assert(mean(List(-1.0,0.0,1.0)) == 0.0)
    assert(mean(List(1.0,2.0,3.0)) == 2.0)
    assert(mean(List(0.1,0.3,0.5)) == 0.3)
  }

}
