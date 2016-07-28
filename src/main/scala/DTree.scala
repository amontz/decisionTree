package randomForest

import math.log

sealed trait DTree[+A]
case class Leaf[A](value: A) extends DTree[A]
case class Branch[A](left: DTree[A], right: DTree[A]) extends DTree[A]

object DTree {

  def log2(x: Double) = log(x)/log(2)
}
