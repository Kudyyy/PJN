package pjn.lab1

import java.io.{File, PrintWriter}

import play.api.libs.json._


object Cacher{

  def getLanguages(list: Array[File]): Set[String] = {
    list.map( x => "/([A-Za-z]+)".r.findFirstIn(x.toString).getOrElse("/").substring(1)).toSet
  }

  def main(args: Array[String]): Unit = {
    val filesList = new File("files").listFiles()
    val languages = getLanguages(filesList).map( l => l -> filesList.filter(_.toString.contains(l)).map(_.toString).toList).toMap
    for ( i <- 2 to 7){
      println(s"Doing $i-grams")
      val ngrams_mapped = languages.map( lang => {
        println("\t" + lang._1)
        lang._1 -> NGramMaker.generateNGramsMap(lang._1, lang._2, i)
      })
      val js =  JsObject(ngrams_mapped.map(x => x._1 -> JsObject(x._2.map( z => z._1 -> JsNumber(z._2)))))
      new PrintWriter(s"cache/${i}Gram.json") {
        write(js.toString())
        close()
      }
    }
  }

}
