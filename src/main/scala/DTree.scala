package randomForest

import math.log

// type split = (Int, Int) : (feature, value) = (index, level)

sealed trait DTree[+A]
case class Leaf[A](value: A) extends DTree[A]
case class Branch[A](left: DTree[A], right: DTree[A], split: (Int, Int)) extends DTree[A]

object DTree {

  def fit(data: Vector[Vector[Int]]): DTree[Double] = {
    val split = getSplit(data)
    Leaf(1.0) // remove
  }

  def getSplit(data: Vector[Vector[Int]]): (Int, Int) = {
    (0,0)
  }

  def bestSplitFeature(v: Vector[Int]): (Int, Double) = {
    v.distinct
     .map(l => (l, infoGain(v, v.partition(a => a == l))))
     .maxBy(_._2)
  }

  def infoGain(p: Vector[Int], c: (Vector[Int], Vector[Int])): Double = {
    val n = p.length.toDouble

    //TODO: no map for tuple, how to better handle this?
    entropy(p) - c._1.length.toDouble/n*entropy(c._1) - c._2.length.toDouble/n*entropy(c._2)
  }

  def entropy(v: Vector[Int]): Double = {
    v.groupBy(x => x)                             // returns a Map, i.e., tuple (val, Vector(...))
     .mapValues(gv => gv.length.toDouble/v.length)
     .mapValues(p => -p*log2(p))
     .foldLeft(0.0)(_ + _._2)                     // left is double, right is "Map"
                                                  // reverse order for foldRight
                                                  // how to use fold?
  }

  def log2(x: Double): Double = log(x)/log(2)
}
