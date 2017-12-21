package scorer

import scala.collection.mutable.ListBuffer


trait NgramScorer {

  val ngrams = 0
  val _SMALL = 1e-20
  def contingency(marginals: Seq[Int]): Seq[Int]
  def marginals(contingency: Seq[Int]): Seq[Int]

  // Calculates expected values for a contingency table.
  def expectedValues(cont: Seq[Int]): Seq[Double] = {
    var n_all = 0
    for (i <- cont) {
      n_all += i
    }

    var bits = ListBuffer[Int]()
    for (i <- List.range(0, cont.length)) {
      bits += 1 << i
    }

    var res = ListBuffer[Double]()

    // for each contingency table cell, compute expected value
    val denom = scala.math.pow(n_all, (ngrams - 1))
    for (i <- List.range(0, cont.length)) {
      var prodSumCont = 1
      for(j <- bits) {
        var sumCont = 0
        for(x <- List.range(0, math.pow(2, ngrams).toInt)) {
          if((x & j) == (i & j)) {
            sumCont += cont(x)
          }
        }
        prodSumCont = prodSumCont * sumCont
      }
      res += prodSumCont/denom
    }
    return res
  }

  def chiSq(marginals: Seq[Int]): Double = {
    val cont = contingency(marginals)
    val exps = expectedValues(cont)

    var res = 0.0d
    for(i <- 0 to cont.length-1) {
      val obs = cont(i)
      val exp = exps(i)
      res += math.pow((obs-exp), 2)/(exp + _SMALL)
    }
    res
  }

  def likelihoodRatio(marginals: Seq[Int]): Double = {

    val cont = contingency(marginals)
    val exps = expectedValues(cont)
    var res = 0.0d
    for(i <- 0 to cont.length-1) {
      val obs = cont(i)
      val exp = exps(i)

      res += obs*math.log((obs/(exp+_SMALL)) + _SMALL)
    }
    res*ngrams
  }
}
