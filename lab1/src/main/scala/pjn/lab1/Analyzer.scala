package pjn.lab1

import java.io.File
import play.api.libs.json._
import scala.io.Source
import scala.math.{pow, sqrt}

object Analyzer  {

  private final val boundaries = List(0.2, 0.5, 0.8, 0.9)

  def getLanguages(list: Array[File]): Set[String] = {
    list.map( x => "/([A-Za-z]+)".r.findFirstIn(x.toString).getOrElse("/").substring(1)).toSet
  }

  def calcNorms(js: JsValue, languages: Set[String], under_test: Map[String, Int]): Map[String, Double] = {
    languages.map(lang => {
      var sum_base: Double = 0
      var sum_test: Double = 0
      var sum_both: Double = 0

      (js \ lang).as[Map[String, Int]].foreach(x => {
        val base_val = x._2.toDouble
        val tst_val = under_test.getOrElse(x._1, 0).toDouble
        sum_base += pow(base_val, 2)
        sum_test += pow(tst_val, 2)
        sum_both += base_val * tst_val
      })
      lang -> (1 - (sum_both/(sqrt(sum_test)* sqrt(sum_base))))
    }).toMap
  }

  def calcStats(norms: Map[String, Double], language: String): Map[Double, Map[String, Double]] = {
    boundaries.map(boundary => {
      var true_positive: Double = 0
      var true_negative: Double = 0
      var false_positive: Double = 0
      var false_negative: Double = 0
      norms.foreach(norm => {
        if (language == norm._1 && norm._2 <= boundary)
          true_positive += 1
        if (language != norm._1 && !(norm._2 <= boundary))
          true_negative += 1
        if (language != norm._1 && norm._2 <= boundary)
          false_positive += 1
        if (language == norm._1 && !(norm._2 <= boundary))
          false_negative += 1
      })
      val precision = true_positive / (true_positive + false_positive)
      val recall = true_positive / (true_positive + false_negative)
      val f1 = 2 * (precision * recall) / (precision + recall)
      val accuracy = (true_positive + true_negative) / (true_positive + true_negative + false_positive + false_negative)
      boundary -> Map("precision" -> precision,"recall" ->  recall, "f1" ->  f1,"accuracy" ->  accuracy)
    }).toMap
  }

  def main(args: Array[String]): Unit = {

    val filesList = new File("test_files").listFiles()

    for (n <- 2 to 7){
      println(s"$n-grams:")
      val source: String = Source.fromFile(s"cache/${n}Gram.json", "ISO-8859-1").getLines.mkString
      val json: JsValue = Json.parse(source)
      getLanguages(filesList).foreach(lang => {
        println(s"\t$lang")
        val ngram_test = NGramMaker.generateNGramsMap(lang, List(s"test_files/${lang}.txt"), n)
        val norms = calcNorms(json, json.as[JsObject].keys.toSet, ngram_test)
        val stats = calcStats(norms, lang)
        println("\t\tNorms:")
        norms.toSeq.sortBy(_._2).foreach(x => {
          println(s"\t\t${x._1}: ${x._2}")
        })
        println("\t\tStats:")
        stats.foreach(stat => {
          println(s"\t\t\tBoundary = ${stat._1}:")
          stat._2.foreach(measure => println(s"\t\t\t\t${measure._1} = ${measure._2}"))
        })
      })
    }

  }

}
