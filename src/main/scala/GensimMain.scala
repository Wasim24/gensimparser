import java.util.regex.Pattern

import scala.collection.mutable.ListBuffer

object GensimMain {

  def main(args: Array[String]): Unit = {
    val descriptions = ListBuffer("Microsoft Corporation pvt ltd, incorporated on September 22, 1993, is a technology company. The Company develops, licenses, and supports a range of software products, services and devices. ",
    "Microsoft Corporation pvt ltd, incorporated on September 22, 1993, is a technology company. The Company develops, licenses, and supports a range of software products, services and devices.",
    "Microsoft Corporation pvt ltd, incorporated on September 22, 1993, is a technology company. The Company develops, licenses, and supports a range of software products, services and devices.",
    "Microsoft Corporation pvt ltd, incorporated on September 22, 1993, is a technology company. The Company develops, licenses, and supports a range of software products, services and devices.")

    var cleanDescriptions = ListBuffer[String]()
    for(s <- descriptions) {
      var p = Pattern.compile("([\\`\\~\\!\\@\\#\\$\\%\\^\\&\\*\\(\\)\\.\\_\\-\\+\\=\\{\\}\\(\\),\\;\\:\\\"\\'\\?])")
      var m = p.matcher(s);
      while(m.find()){
        cleanDescriptions += m.replaceAll(" $1 ")
      }
    }

    val phraser = new Phraser(2, 2)
    phraser.init()

    phraser.addVocab(cleanDescriptions)
    println("Vocab learnt:", phraser.vocab)

    val t = phraser.exportPhrases(cleanDescriptions)
    for((words, score) <- t) {
      println(words, ":", score)
    }

    //words_stream, min_count=2, threshold=2)
  }
}
