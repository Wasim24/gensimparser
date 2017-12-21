import java.util

import scorer._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

// https://www.ranks.nl/stopwords
case class Phraser(minCount: Int = 5, threshold: Float = 10, common_terms: util.HashSet[String] = new util.HashSet[String](), max_vocab_size: Int = 40000000, delimiter: String = "_", progress_per: Int =50) {
  // scorer: Scorer=DefaultScorer) {


  type VOCAB_TYPE = mutable.HashMap[String, Int]

  var corpus_word_count: Long = 0
  var min_reduce: Int = 0
  var sentence_no = 0
  var vocab: mutable.HashMap[String, Int] = new mutable.HashMap[String, Int]()

  def init(vocab: Option[VOCAB_TYPE] = None): Unit = {
    if (minCount <= 0) {
      throw new RuntimeException("min_count should be at least 1")
    }

    if(vocab.isDefined) {
      this.vocab = vocab.get
    }
    /* if(threshold <= 0 && this.scorer == DefaultScorer) {
       throw new RuntimeException("threshold should be positive for default scoring")
     }
     if( this.scorer == NpmiScorer && (threshold < -1 || threshold > 1)) {
       throw new RuntimeException("threshold should be between -1 and 1 for npmi scoring")
     }*/
  }

  private def addWordToVocab(vocab: mutable.HashMap[String, Int], word: String, count: Int = 1): Unit = {
    val wordCount = if(vocab.contains(word)) vocab.getOrElse(word, 0) + 1 else 1
    vocab.put(word, wordCount)
  }

  private def pruneVocab(vocab: mutable.HashMap[String, Int], minCount: Int): Unit = {

    var keys = vocab.keySet
    for(key <- keys) {
      if(vocab.getOrElse(key, 0)  < minCount) {
        vocab.remove(key)
      }
    }
  }

  private def nGramsFromSentence(sentence: String): (VOCAB_TYPE, Int) = {
    var sentence_vocab = new VOCAB_TYPE()
    var last_uncommon: Option[String] = None
    var total_words = 0
    var in_between = scala.collection.mutable.ListBuffer[String]()
    val words = sentence.split(" ").filter(w => w.size > 1)
    for (word: String <- words) {

      val is_common = common_terms.contains(word)
      if (!is_common) {

        // add word in vocab  as 1gram
        addWordToVocab(sentence_vocab, word)

        // nGrams
        if (last_uncommon.isDefined) {
          var components = new StringBuffer()
          components.append(last_uncommon.get + "_")
          if (in_between.size > 0) {
            components.append(in_between.mkString(delimiter) + "_")
          }
          components.append(word)

          addWordToVocab(sentence_vocab, components.toString) //bigram
          /*if(in_between.size == 0) {
            addWordToVocab(bfd, components.toString) //bigram
          } else {
            addWordToVocab(tfd, components.toString) // trigram
          }*/

          // chk if 3Gram then clear i.e., slide window. Else, add word to build higher 3Grams
          val isNGram = (in_between.size + 2 == 3)
          if (isNGram) {
            last_uncommon = Some(word)
            in_between.clear()
          } else {
            in_between.+=(word)
          }
        } else {
          last_uncommon = Some(word)
        }
      }
      else if (last_uncommon.isDefined) {
        in_between.+=(word)
      }
      total_words += 1
    }
    (sentence_vocab, total_words)
  }

  private def learnVocab(sentences: ListBuffer[String], max_vocab_size: Int, delimiter: String="_", progress_per: Int =10000, common_terms: util.HashSet[String]): (Int, mutable.HashMap[String, Int], Int) = {

    """Collect unigram/bigram counts from the `sentences` iterable."""

    // var sentence_no = -1
    var total_words = 0

    println("collecting all words and their counts:" + sentences.size)
    var vocab_for_given_sentences = new mutable.HashMap[String, Int]()
    var min_reduce = 1
    for (sentence <- sentences) {
      sentence_no = sentence_no + 1
      if (sentence_no % progress_per == 0) {
        println("PROGRESS: at sentence #%d, processed %d words and %d word types".format(sentence_no, total_words, vocab.size))
      }

      // get nGrams for sentence
      val sentenceNGrams = nGramsFromSentence(sentence)
      total_words += sentenceNGrams._2
      val sentence_vocab = sentenceNGrams._1
      for ((word, count) <- sentence_vocab) {
        addWordToVocab(vocab_for_given_sentences, word, count)
      }

      if(vocab_for_given_sentences.size > max_vocab_size) {
        pruneVocab(vocab_for_given_sentences, min_reduce)
        min_reduce += 1
      }
    }
    println("collected %d word types from a corpus of %d words (unigram + bigrams) and %d sentences".format(vocab_for_given_sentences.size, total_words, sentence_no + 1))
    return (min_reduce, vocab_for_given_sentences, total_words)
  }

  def addVocab(sentences: ListBuffer[String]) {
    /*
        Merge the collected counts `vocab` into this phrase detector.
        uses a separate vocab to collect the token counts from `sentences`.
        this consumes more RAM than merging new sentences into `self.vocab`
        directly, but gives the new sentences a fighting chance to collect
        sufficient counts, before being pruned out by the (large) accummulated
        counts collected in previous learn_vocab runs.
    */
    var res = this.learnVocab(sentences, this.max_vocab_size, this.delimiter, this.progress_per, this.common_terms)

    var min_reduce = res._1
    var vocab_for_given_sentences = res._2
    var total_words = res._3

    this.corpus_word_count += total_words
    if(!this.vocab.isEmpty) {
      // println("merging %d counts into %d".format(vocab_for_given_sentences.size, vocab.size))
      this.min_reduce = Math.max(this.min_reduce, min_reduce)
      for ((word, count) <- vocab_for_given_sentences) {
        addWordToVocab(this.vocab, word, count)
      }
      if (this.vocab.size > this.max_vocab_size) {
        pruneVocab(this.vocab, this.min_reduce)
        this.min_reduce += 1
      }
      // println("merged vocab")
    }
    else {
      // in common case, avoid doubling gigantic dict
      // println("using %d counts as vocab in %s".format(vocab.size, this.toString))
      this.vocab = vocab_for_given_sentences
    }
  }

  def exportPhrasesAsTuples(sentences: ListBuffer[String], out_delimiter: String=" "): mutable.HashMap[String, Double] = {
    var phrases = exportPhrases(sentences)
    var res = new mutable.HashMap[String, Double]()
    for((word: String, score: Double) <- phrases) {
      res.put(word.split(this.delimiter).mkString(out_delimiter), score)
    }
    return res
  }

  def exportPhrases(sentences: ListBuffer[String]): mutable.HashMap[String, Double] = {
    """
        Generate an iterator that contains all phrases in given 'sentences'

        Example::

          >>> sentences = Text8Corpus(path_to_corpus)
          >>> bigram = Phrases(sentences, min_count=5, threshold=100)
          >>> for phrase, score in bigram.export_phrases(sentences):
          ...     print(u'{0}\t{1}'.format(phrase, score))

            then you can debug the threshold with generated tsv
        """

    var res = new mutable.HashMap[String, Double]()
    for(sentence <- sentences) {
      val ngrams_fd = nGramsFromSentence(sentence)._1
      val n_all = this.vocab.size

      for(ngramFd <- ngrams_fd) {

        val word = ngramFd._1
        val count = ngramFd._2
        val is_unigram = !word.contains(this.delimiter)

        if(!is_unigram) {

          val ngrams = word.split(this.delimiter)
          val is_bigram = ngrams.length == 2
          val is_trigram = ngrams.length == 3

          if(is_bigram) {
            val word_1 = ngrams(0)
            val word_2 = ngrams(1)
            val n_1 = ngrams_fd.getOrElse(word_1, 0)
            val n_2 = ngrams_fd.getOrElse(word_2, 0)
            val n_12 = ngrams_fd.getOrElse(word, 0)
            val score = BigramScorer.chiSq(n_12, n_1, n_2, n_all)
            res.put(word, score)
          } else if(is_trigram) {
            val word_1 = ngrams(0)
            val word_2 = ngrams(1)
            val word_3 = ngrams(1)
            val n_1 = ngrams_fd.getOrElse(word_1, 0)
            val n_2 = ngrams_fd.getOrElse(word_2, 0)
            val n_3 = ngrams_fd.getOrElse(word_3, 0)
            val n_12 = ngrams_fd.getOrElse(word_1 + delimiter + word_2, 0)
            val n_13 = ngrams_fd.getOrElse(word_1 + delimiter + word_3, 0)
            val n_23 = ngrams_fd.getOrElse(word_2 + delimiter + word_3, 0)
            val n_123 = ngrams_fd.getOrElse(word, 0)
            val score = TrigramScorer.chiSq(Seq(n_123, n_12, n_13, n_23, n_1, n_2, n_3, n_all))
            res.put(word, score)
          }
        }
      }

      /* var bigrams = SentenceAnalyzer.analyzeNSentence(4, this.vocab, sentence.split(" "), this.common_terms, this.scorer, this.minCount, this.threshold)

       // keeps only not None scores
       for((bigram: ListBuffer[String], score: Option[Double]) <- bigrams) {
         if(score.isDefined) {
           res.put(bigram, score.getOrElse(0))
         }
       }*/
    }
    return res
  }
}