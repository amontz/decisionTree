package decisionTree

import math.log

// type split = (Int, Int) : (feature, value) = (index, level)

sealed trait DTree[+A]
case class Leaf[A](value: A) extends DTree[A]
case class Branch[A](left: DTree[A], right: DTree[A], split: (Int, Int)) extends DTree[A]

object DTree {

  def predict(record: Vector[Int], dt: DTree[Double]):Double = dt match {
    case Leaf(x) => x
    case Branch(l, r, s) => {
      if (record(s._1) == s._2) predict(record, l)
      else predict(record, r)
    }
  }

  def fit(data: Vector[Vector[Int]]): DTree[Double] = fit(data, 8)

  def fit(data: Vector[Vector[Int]], depth: Int): DTree[Double] = {
    if (depth == 0) Leaf(mean(data.map(r => r.last)))
    else {
      val split = getSplit(data)
      val (ls, rs) = data.partition(r => r(split._1) == split._2)
      lazy val lt = ls.map(r => r.last)
      lazy val rt = rs.map(r => r.last)
      lazy val l = if (entropy(lt) == 0) Leaf(mean(lt)) else fit(ls, depth-1)
      lazy val r = if (entropy(rt) == 0) Leaf(mean(rt)) else fit(rs, depth-1)
      Branch(l, r, split)
    }
  }

  def mean(v: Vector[Int]): Double = v.foldLeft(0)(_ + _) / v.length.toDouble

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
