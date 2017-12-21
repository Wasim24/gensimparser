package scorer

import scala.collection.mutable.ListBuffer

object TrigramScorer extends NgramScorer {

  val ngram = 3

  // Calculates values of a trigram contingency table from marginal values
  override def contingency(marginals: Seq[Int]): Seq[Int] = {

    val n_123 = marginals(0)
    val n_12 = marginals(1)
    val n_13 = marginals(2)
    val n_23 = marginals(3)
    val n_1 = marginals(4)
    val n_2 = marginals(5)
    val n_3 = marginals(6)
    val n_all = marginals(7)

    val n_23without1 = n_23 - n_123
    val n_13without2 = n_13 - n_123
    val n_12without3 = n_12 - n_123

    val n_3without12 = n_3 - n_123 - n_23without1 - n_13without2
    val n_2without13 = n_2 - n_123 - n_23without1 - n_12without3
    val n_1without23 = n_1 - n_123 - n_13without2 - n_12without3

    val n_others = n_all - n_123 - n_23without1 - n_13without2 - n_12without3 - n_3without12 - n_2without13 - n_1without23

    return ListBuffer(n_123, n_23without1, n_13without2, n_3without12, n_12without3, n_2without13, n_1without23, n_others)
  }

  // Calculates values of contingency table marginals from its values
  override def marginals(contingency: Seq[Int]): Seq[Int] = {
    val n_123 = contingency(0)
    val n_23without1 = contingency(1)
    val n_13without2 = contingency(2)
    val n_3without12 = contingency(3)
    val n_12without3 = contingency(4)
    val n_2without13 = contingency(5)
    val n_1without23 = contingency(6)
    val n_others = contingency(7)

    return ListBuffer[Int](n_123,
      (n_123 + n_12without3),
      (n_123 + n_13without2),
      (n_123 + n_23without1),
      (n_123 + n_13without2 + n_12without3 + n_1without23),
      (n_123 + n_23without1 + n_12without3 + n_2without13),
      (n_123 + n_23without1 + n_13without2 + n_3without12),
      (n_123 + n_23without1 + n_13without2 + n_12without3 + n_1without23 + n_2without13 + n_3without12))
  }

  /*override def expectedValues(cont: Seq[Int]): Seq[Double] = {

    var n_all = 0
    for (c <- cont) {
      n_all += c
    }

    var res = ListBuffer[Double]()
    for (i <- 0 to 4) {
      res += ((cont(i) + cont(i ^ 1)) * (cont(i) + cont(i ^ 2))) / n_all
    }
    res
  }*/
}
