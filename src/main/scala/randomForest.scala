package decisionTree

import math._
import util.Random._

object RandomForest {

  def predict(record: Vector[Int], rf: List[DTree[Double]]):Double = {
    mean(rf.map(dt => DTree.predict(record, dt)))
  }

  def randomForest(data: Vector[Vector[Int]],
                   ntree: Int=1,
                   depth: Int=8): List[DTree[Double]] = {

    val nrow = data.length
    val ncol = data(0).length - 1
    val mtry = floor(pow(ncol, 0.5)).toInt
    val sampSize = ceil(0.632*nrow).toInt
    randomForest(data, ntree, depth, mtry, sampSize)
  }

  def randomForest(data: Vector[Vector[Int]],
                   ntree: Int,
                   depth: Int,
                   mtry: Int,
                   sampSize: Int): List[DTree[Double]] = {

    (0 until ntree).map(i => DTree.fit(subSample(data, sampSize), depth, mtry)).toList
  }

  /**
    * Return a subsample of data, with sampSize rows, with replacement
    */
  def subSample(data: Vector[Vector[Int]], sampSize: Int): Vector[Vector[Int]] = {
    Seq.fill(sampSize)(nextInt(data.length)).sorted
    .map(i => data(i)).toVector
  }

  def mean(v: List[Double]): Double = {
    if (v.length.toDouble == 0) 0.0
    else v.foldLeft(0.0)(_ + _) / v.length.toDouble
  }
}
