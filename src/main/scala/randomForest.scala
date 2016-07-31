package decisionTree

import util.Random.shuffle

object RandomForest {

  def predict(record: Vector[Int], rf: List[DTree[Double]]):Double = {
    mean(rf.map(dt => DTree.predict(record, dt)))
  }

  def randomForest(data: Vector[Vector[Int]],
                   ntree: Int=1,
                   depth: Int=8,
                   mtry: Int,
                   sampSize: Int): List[DTree[Double]] = {

    (0 until ntree).map(i => DTree.fit(subSample(data, sampSize), depth, mtry)).toList
  }

  def subSample(data: Vector[Vector[Int]], sampSize: Int): Vector[Vector[Int]] = {
    shuffle((0 until data.length).toList).take(sampSize).sorted
    .map(i => data(i)).toVector
  }

  def mean(v: List[Double]): Double = v.foldLeft(0.0)(_ + _) / v.length.toDouble
}
