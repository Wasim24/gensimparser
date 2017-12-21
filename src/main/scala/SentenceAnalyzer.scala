import scorer.Scorer

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object SentenceAnalyzer {

  val delimiter = "_"

  def scoreItem(vocab: mutable.HashMap[String, Int], worda: String, wordb: String, components: ListBuffer[String], scorer: Scorer, minCount: Int): Double = {

    if (vocab.contains(worda) && vocab.contains(wordb)) {
      val bigram = components.mkString(this.delimiter)
      if(vocab.contains(bigram)) {
        return scorer.score(vocab.getOrElse(worda, 0), vocab.getOrElse(wordb, 0), vocab.getOrElse(bigram, 0), vocab.size, minCount, 0)
      }
    }
    return -1
  }

  /**
    * @param sentence a token list representing the sentence to be analyzed.
    * @param threshold the minimum score for a bigram to be taken into account
    * @param commonTerms the list of common terms, they have a special treatment
    * @param scorer the scorer function, as given to Phrases
    */
  def analyzeNSentence(n: Int, vocab: mutable.HashMap[String, Int], sentence: Array[String], commonTerms: java.util.HashSet[String], scorer: Scorer, minCount: Int, threshold: Float): mutable.HashMap[ListBuffer[String], Option[Double]] = {
    var last_uncommon: Option[String] = None
    var in_between = ListBuffer[String]()
    var in_between_uncommon_count = 0
    val nGrams = Math.max(n, 2)
    var res = new mutable.HashMap[ListBuffer[String], Option[Double]]()
    for (word <- sentence) {
      val is_common = commonTerms.contains(word)
      if (!is_common) {
        if(!last_uncommon.isDefined) {
          last_uncommon = Some(word)
        } else {
          if(in_between_uncommon_count < n-2) {
            in_between += word
            in_between_uncommon_count = in_between_uncommon_count + 1
          } else {
            // form chain of lastuncommon + inbetween + word
            var chain = ListBuffer[String]()
            chain += last_uncommon.get
            for (w <- in_between) {
              chain += w
            }
            chain += word
            println("chain:", chain.mkString("||"))

            val score = this.scoreItem(vocab, last_uncommon.get, word, chain, scorer, minCount)
            if (score > threshold) {
              res.put(chain, Some(score))
              last_uncommon = None
              in_between.clear()
              in_between_uncommon_count = 0
            } else {
              // release words individually
              res.put(ListBuffer(last_uncommon.get), None)
              for (w <- in_between) {
                res.put(ListBuffer(w), None)
              }
              in_between.clear()
              in_between_uncommon_count = 0
              last_uncommon = Some(word)
            }
          }
        }
      } else {
        // common term
        if (last_uncommon.isDefined) {
          in_between += word
        } else {
          res.put(ListBuffer(word), None)
        }
      }
    }
    return res
  }

  /**
    * @param sentence a token list representing the sentence to be analyzed.
    * @param threshold the minimum score for a bigram to be taken into account
    * @param commonTerms the list of common terms, they have a special treatment
    * @param scorer the scorer function, as given to Phrases
    */
  def analyzeSentence(vocab: mutable.HashMap[String, Int], sentence: Array[String], commonTerms: java.util.HashSet[String], scorer: Scorer, minCount: Int, threshold: Float): mutable.HashMap[ListBuffer[String], Option[Double]] = {
    var last_uncommon: Option[String] = None
    var in_between = ListBuffer[String]()
    var res = new mutable.HashMap[ListBuffer[String], Option[Double]]()
    for (word <- sentence) {
      val is_common = commonTerms.contains(word)
      if (!is_common && last_uncommon.isDefined) {

        // form chain of lastuncommon + inbetween + word
        var chain = ListBuffer[String]()
        chain += last_uncommon.get
        for (w <- in_between) {
          chain += w
        }
        chain += word

        val score = this.scoreItem(vocab, last_uncommon.get, word, chain, scorer, minCount)
        if (score > threshold) {
          res.put(chain, Some(score))
          last_uncommon = None
          in_between.clear()
        } else {
          // release words individually
          res.put(ListBuffer(last_uncommon.get), None)
          for (w <- in_between) {
            res.put(ListBuffer(w), None)
          }
          in_between.clear()
          last_uncommon = Some(word)
        }
      } else if (!is_common) {
        last_uncommon = Some(word)
      } else {
        // common term
        if (last_uncommon.isDefined) {
          in_between += word
        } else {
          res.put(ListBuffer(word), None)
        }
      }
    }
    return res
  }
}
