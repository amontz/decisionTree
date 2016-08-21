import io.Source
import decisionTree._

object titanic {

  def main(args: Array[String]): Unit = {
    val f = Source.fromFile("./titanic_design_matrix.csv")
    val dm = f.getLines().drop(1).map(_.split(",")).map(_.toVector.map(_.toInt)).toVector

    val rf = RandomForest.randomForest(dm, 100, 8, 2, dm.length)

    val ncol = dm(0).length
    val predData = dm.map(r => r.take(ncol - 1))
    val preds = RandomForest.predict(predData, rf)
    print(preds)
    print("\n")
  }
 }
