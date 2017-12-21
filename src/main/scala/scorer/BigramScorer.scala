package scorer

import scala.collection.mutable.ListBuffer

object BigramScorer extends NgramScorer {

  val ngram = 2

  // Calculates values of a bigram contingency table from marginal values
  override def contingency(marginals: Seq[Int]): Seq[Int] = {

    val n_1and2 = marginals(0)
    val n_1 = marginals(1)
    val n_2 = marginals(2)
    val n_all = marginals(3)

    val n_2without1 = n_2 - n_1and2
    val n_1without2 = n_1 - n_1and2
    val n_others = n_all - n_1and2 - n_2without1 - n_1without2

    return ListBuffer(n_1and2, n_2without1, n_1without2, n_others)

  }

  // Calculates values of contingency table marginals from its values
  override def marginals(contingency: Seq[Int]): Seq[Int] = {

    val n_1and2 = contingency(0)
    val n_2without1 = contingency(1)
    val n_1without2 = contingency(2)
    val n_others = contingency(3)

    val n_2 = n_2without1 + n_1and2
    val n_1 = n_1without2 + n_1and2
    val n_all = n_others + n_2without1 + n_1without2 + n_1and2
    return ListBuffer(n_1and2, n_1, n_2, n_all)
  }

  override def expectedValues(cont: Seq[Int]): Seq[Double] = {

    var n_all = 0
    for (c <- cont) {
      n_all += c
    }

    var res = ListBuffer[Double]()
    for (i <- 0 to 4) {
      res += ((cont(i) + cont(i ^ 1)) * (cont(i) + cont(i ^ 2))) / n_all
    }
    res
  }

  def phiSq(marginals: Seq[Int]): Double = {
    val cont = contingency(marginals)
    val n_1and2 = cont(0)
    val n_2without1 = cont(1)
    val n_1without2 = cont(2)
    val n_others = cont(3)

    val numerator = ((n_1and2 * n_others) - (n_1without2 * n_2without1))
    val denominator = (n_1and2 + n_1without2) * ((n_1and2 + n_2without1) * (n_others + n_1without2) * (n_others + n_2without1))
    return math.pow(numerator, 2) / (denominator)
  }

  def chiSq(n_1and2: Int, n_1: Int, n_2: Int, n_all: Int): Double = {
    return n_all * phiSq(ListBuffer(n_1and2, n_1, n_2, n_all))
  }
}
