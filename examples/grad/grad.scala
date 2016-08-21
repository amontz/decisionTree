import io.Source
import decisionTree._

object grad {

  def main(args: Array[String]): Unit = {
    val train = Source.fromFile("./grad_train.csv")
    val test = Source.fromFile("./grad_test.csv")
    val train_dm = train.getLines().drop(1).map(_.split(",")).map(_.toVector.map(_.toInt)).toVector
    val test_dm = test.getLines().drop(1).map(_.split(",")).map(_.toVector.map(_.toInt)).toVector

    val rf = RandomForest.randomForest(train_dm, 100, 8, 4, train_dm.length)

    val ncol = test_dm(0).length
    val predData = test_dm.map(r => r.take(ncol - 1))
    val preds = RandomForest.predict(predData, rf)
    print(preds)
    print("\n")
  }
 }
