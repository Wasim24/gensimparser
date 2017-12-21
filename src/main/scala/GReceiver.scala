import org.apache.spark.scheduler.{SparkListenerJobEnd, SparkListenerTaskEnd}

object GReceiver {


import org.apache.log4j.{Level, Logger}
import org.apache.spark.scheduler.{SparkListener, SparkListenerApplicationEnd}
import org.apache.spark.sql.SparkSession

import scala.collection.mutable.ListBuffer

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

  def main(args: Array[String]): Unit = {
    Logger.getLogger("org").setLevel(Level.OFF)
    Logger.getLogger("akka").setLevel(Level.OFF)

    var inputfile = "/Users/surthi/Downloads/full100.json" // "s3n://newsplatform-config-test/news_analytics/full100.json"
    args.foreach(arg => {
      if(arg.startsWith("inputfile=")) {
        val userinputfile = arg.replace("inputfile=", "")
        if(userinputfile != null && userinputfile.trim() !=  "") {
          inputfile = userinputfile
          println("SETTING USER INPUT TO" + inputfile)
        }
      }
    })
    val spark = SparkSession
      .builder
      .master("local[2]")
      .appName("GensimProcessor")
      .getOrCreate()

    val sc = spark.sparkContext
    import spark.implicits._

    // /Users/surthi/nltk_data/corpora/stopwords/long_stopwords.txt
    val stopwords = sc.broadcast(spark.read.csv("s3n://newsplatform-config-test/long_stopwords.txt").as[String].collect())
    println(stopwords.value.mkString("||"))
    val phraser = new Phraser(2, 2)
    phraser.init()
    val phraserBc = sc.broadcast(phraser)
    // SparkAppListener.phraserBc = phraserBc

    // spark.sparkContext.getConf.set("spark.extraListeners", "com.insideview.news.SparkAppListener")

    spark.sparkContext.addSparkListener(new SparkListener {
      override def onJobEnd(jobEnd: SparkListenerJobEnd): Unit = {
        println("JOB END")
        // vocabDump()
        super.onJobEnd(jobEnd)
      }

      override def onTaskEnd(taskEnd: SparkListenerTaskEnd): Unit = {
        println("TASK END")
        // vocabDump()
        super.onTaskEnd(taskEnd)
      }

      override def onApplicationEnd(applicationEnd: SparkListenerApplicationEnd): Unit = {
        println("APPLICATION END")
        vocabDump()
        super.onApplicationEnd(applicationEnd)
      }

      def vocabDump(): Unit = {
        println("Vocab at end", phraserBc.value.vocab)
        import java.io._
        // /Users/surthi/Downloads/genim_phraser_vocab.txt
        val pw = new PrintWriter(new File("s3n://newsplatform-config-test/gensim_phraser_vocab.txt"))
        for((k,v) <- phraserBc.value.vocab) {
          pw.write(k+"->"+v+"\n")
        }
        pw.close()
      }
    })

    val articles = spark.read.json(inputfile) // "/Users/surthi/Downloads/articles_few.json")
    val enArticles = articles.filter(articles("language") === "English")
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

    enArticles.foreach(x => {
      val sentence = x(1).toString
      val phraser = phraserBc.value
      phraser.addVocab(ListBuffer[String](sentence))
      println(x(1))
    })
  }
}
