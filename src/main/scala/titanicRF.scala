package decisionTree

import io.Source
import DTree._

object titanicRF {

  def main(args: Array[String]): Unit = {
    val f = Source.fromFile("./data/titanic_design_matrix.csv")
    val dm = f.getLines().drop(1).map(_.split(",")).map(_.toVector.map(_.toInt)).toVector

    val rf = RandomForest.randomForest(dm, 100, 8, 6, 20)

    val ncol = dm(0).length
    val predData = dm.map(r => r.take(ncol - 1))
    val preds = RandomForest.predict(predData, rf)
    print(preds)
  }
 }
