package scorer

import scala.math._

trait Scorer {
  def score(wordaCount: Int, wordbCount: Int, bigramCount: Int, vocabLen: Int, minCount: Int, corpusWordCount: Int): Double
}

object NpmiScorer extends Scorer {

  override def score(wordaCount: Int, wordbCount: Int, bigramCount: Int, vocabLen: Int, minCount: Int, corpusWordCount: Int): Double = {
    val pa = wordaCount / corpusWordCount
    val pb = wordbCount / corpusWordCount
    val pab = bigramCount / corpusWordCount
    return log(pab / (pa * pb)) / -log(pab)
  }
}

// chi-square
// loglikelihood
object DefaultScorer extends Scorer {

  override def score(wordaCount: Int, wordbCount: Int, bigramCount: Int, vocabLen: Int, minCount: Int, corpusWordCount: Int = 0): Double = {
    return ((bigramCount - minCount) * vocabLen)/ wordaCount / wordbCount
  }
}
