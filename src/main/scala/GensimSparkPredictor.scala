import org.apache.log4j.{Level, Logger}
import org.apache.spark.sql.SparkSession
import scorer.DefaultScorer

import scala.collection.mutable.ListBuffer
import scala.io.Source

/**
  *1. remove special charecters
  *2. remove non-alphabetic
  *3. Run gensim and get important bigrams
  *3. invoke maxent pos tagger from sentenceNNPTransform
  *4. parse the tagged string. goal is to generate unigrams by tf-idf. For this:
  * 4a. remove all conjunctions and prepositions (_CC and _IN). concat adjacent _NN/__NNS as one phrase
  * 4b. take all _NN and _NNS.
  * 4c. tfidf spark to generate important unigrams
  **/
object GensimSparkPredictor {

  def main(args: Array[String]): Unit = {
    Logger.getLogger("org").setLevel(Level.OFF)
    Logger.getLogger("akka").setLevel(Level.OFF)

    val spark = SparkSession
      .builder
      .master("local[2]")
      .appName("GensimProcessor")
      .getOrCreate()

    val sc = spark.sparkContext
    import spark.implicits._

    val stopwords = sc.broadcast(spark.read.csv("/Users/surthi/nltk_data/corpora/stopwords/long_stopwords.txt").as[String].collect())

    val phraser = new Phraser(2, 2)
    val vocab = new phraser.VOCAB_TYPE()
    val vocab_str = Source.fromFile("/Users/surthi/Downloads/genim_phraser_vocab.txt").mkString
    for(entry <- vocab_str.split("\n")) {
      val kv = entry.split("->")
      vocab.put(kv(0), kv(1).toInt)
    }
    phraser.init(Some(vocab))

    val phraserBc = sc.broadcast(phraser)

    val articles = spark.read.json("/Users/surthi/Downloads/articles_few.json")
    val en_articles = articles.filter(articles("language") === "English")
      .select("articleId", "content", "title")
      .map { x => {
        val content = x.get(1).asInstanceOf[String]//.toLowerCase
        val title = x.get(2).asInstanceOf[String]//.toLowerCase

        var contentAndTitle = title + " " + content
        var regex = "([\\`\\~\\!\\@\\#\\$\\%\\^\\&\\*\\(\\)\\.\\_\\-\\+\\=\\{\\}\\(\\),\\;\\:\\\"\\?])".r
        contentAndTitle = regex.replaceAllIn(contentAndTitle, " ")

        // keep only alphabets and remove stopwords
        contentAndTitle = contentAndTitle.split("\\s+").filter { word => word.matches("^[a-zA-Z]*$") && !stopwords.value.contains(word.toLowerCase) }.mkString(" ").replaceAll("'", "")
        (x.get(0).toString(), contentAndTitle)
      }
      }.select($"_1".as("articleId"), $"_2".as("cleanTitleAndContent"))

      en_articles.foreach(x => {
        val sentence = x(1).toString
        val phraser = phraserBc.value
        println(phraser.exportPhrases(ListBuffer[String](sentence)).toSeq.sortWith(_._2 > _._2).mkString("||"))
        println(SentenceAnalyzer.analyzeSentence(phraser.vocab, sentence.split(" "), phraser.common_terms, DefaultScorer, 0, 1))
        println("==========================")
      })
  }
}
