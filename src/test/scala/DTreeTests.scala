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

  test("best split") {
    assert(bestSplit(Vector(Vector(0,1,0), Vector(1,0,1), Vector(2,1,1))) == (0, 0))
    assert(bestSplit(Vector(Vector(1,0,0), Vector(0,1,1), Vector(1,2,1))) == (1, 0))
  }

  test("mean") {
    assert(mean(Vector(0,0,0)) == 0)
    assert(mean(Vector(-1,0,1)) == 0)
    assert(mean(Vector(1,2,3)) == 2)
  }

  test("fit with depth 0") {
    assert(fit(Vector(Vector(0,0), Vector(1,0)), 0) == Leaf(0.0))
    assert(fit(Vector(Vector(0,0), Vector(1,1)), 0) == Leaf(0.5))
  }

  test("fit") {
    val d1 = Vector(1, 1, 1, 1)
    val d2 = Vector(1, 1, 0, 1)
    val d3 = Vector(0, 0, 1, 2)
    val d4 = Vector(1, 0, 0, 2)
    val d = Vector(d1, d2, d3, d4)
    assert(fit(d) === Branch(Leaf(1.0), Leaf(2.0), (1, 1)))
  }

  test("predict stumps") {
    val lLeaf = Leaf(0.0)
    val rLeaf = Leaf(1.0)
    val root = Branch(lLeaf, rLeaf, (2, 1))

    val record1 = Vector(0, 1, 0)
    assert(predict(record1, root) == 1.0)

    val record2 = Vector(0, 1, 1)
    assert(predict(record2, root) == 0.0)
  }

  test("predict 2-level") {
    val llLeaf = Leaf(0.0)
    val lrLeaf = Leaf(1.0/3.0)
    val lNode = Branch(llLeaf, lrLeaf, (2,1))
    val rlLeaf = Leaf(2.0/3.0)
    val rrLeaf = Leaf(1.0)
    val rNode = Branch(rlLeaf, rrLeaf, (0,0))
    val root = Branch(lNode, rNode, (1,2))

    val record1 = Vector(0, 2, 1)
    assert(predict(record1, root) == 0.0)

    val record2 = Vector(0, 2, 0)
    assert(predict(record2, root) == 1.0/3.0)

    val record3 = Vector(0, 0, 1)
    assert(predict(record3, root) == 2.0/3.0)

    val record4 = Vector(1, 1, 0)
    assert(predict(record4, root) == 1.0)
  }
}
