package pjn.lab1

import scala.io.Source

object NGramMaker{

  def generateNGramsMap(language: String, files: List[String], n: Int): Map[String, Int] = {

    val formattedText = files.foldRight(Map.empty[String, Iterator[String]]) { (path, map) =>
      map + (path -> Source.fromFile(path, "ISO-8859-1").getLines().map(_.replaceAll("[\\!-@\\\t- ]", "")))

    }
    val ngrams = formattedText.map(x => x._2.reduce(_ + _)).map(line => {
      for (i <- 0 until line.length if i <= line.length - n) yield line.substring(i, i + n)
    })
    val mapped_ngrams = ngrams.flatten.foldRight(Map.empty[String, Int]) { (ngram, acc) =>
      acc + (ngram -> (acc.getOrElse(ngram, 0) + 1))
    }
    mapped_ngrams
  }

}
