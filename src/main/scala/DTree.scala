package decisionTree

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

  /** 
    * Return a tuple of the best (feature index, level of that feature index)
    */
  def getSplit(data: Vector[Vector[Int]]): (Int, Int) = {
    val idx = 0 until data.length - 1
    val s = idx.map(i => (i, bestSplitFeature(data.map(r => Vector(r(i), r.last)))))
              .maxBy(_._2._2) // (feature, (level, infoGain))
    (s._1, s._2._1)           // drop info gain
  }

  /** 
    * Return a tuple of the best (level of this feature index, info gain)
    * 
    *  v: n x 2 matrix. First col is feature, second is target
    */
  def bestSplitFeature(v: Vector[Vector[Int]]): (Int, Double) = {
    v.map(r => r(0)).distinct
     .map(l => (l, v.partition(r => r(0) == l))) // Vector(tuples(Vector(Vector)))!
     .map(t => (t._1, infoGain(v.map(r => r.last), t._2._1.map(r => r.last), t._2._2.map(r => r.last))))
     .maxBy(_._2)
  }
 
  def infoGain(p: Vector[Int], c1: Vector[Int], c2: Vector[Int]): Double = {
    val n = p.length.toDouble

    //TODO: no map for tuple, how to better handle this?
    entropy(p) - c1.length.toDouble/n*entropy(c1) - c2.length.toDouble/n*entropy(c2)
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
