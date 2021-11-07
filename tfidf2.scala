import scala.io.Source
import scala.io.StdIn.readLine
import scala.collection.mutable.Map
import scala.collection.immutable.ListMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.LinkedHashMap
import scala.util.control.Exception._
import util.control.Breaks._
import java.io._

class Input:
    val stopwords = Source.fromFile("stop_words_english.txt").getLines.toList
    var temp: ListBuffer[String] = ListBuffer() // help list to read words
    var contents = Map [String, Int]() //count of all words from all docs
    var docs_tfidf = Map[String, ListMap[String, Double]]() // tf.idf of words grouped by docs
    var docs_tf = Map[String, ListMap[String, Float]]() // tf of words grouped by docs
    var docs_count = Map [String, ListMap[String, Int]]() // count of words grouped by docs
    var idf = Map[String, Int]() //number of docs in which word appears 

    def addFile (filename:String) = 
        getFromFile(filename)
        removeStopwords()
        docs_count.addOne(filename, countWord())        
        clearTemp()

    def sumUp (n : Int) = 
        var res = ListMap(contents.toSeq.sortWith(_._2 > _._2): _*)
        println("All documents - " + res.dropRight(res.size-n).keys.mkString(", "))

    def getFromFile(filename: String) = 
      temp.addAll(Source.fromFile(filename).getLines.flatMap(line => line.split("""\W+""")).toArray)

    def removeStopwords() = 
    temp
    .map(_.toLowerCase())
    .toList
    .filter(! stopwords.contains(_))
    .filterNot(_ == "")

    def clearTemp () = 
      temp.clear()

    def countWord () : ListMap[String, Int] = {
        var count = scala.collection.mutable.Map[String, Int]()
        var words = removeStopwords()
        for word <- words do
            if (count.contains(word)) then count(word) += 1;
            else count.addOne(word -> 1)
            if (contents.contains(word)) then contents(word) += 1;
            else contents.addOne(word -> 1)
            if (idf.contains(word)) then idf(word) += 1;
            else idf.addOne(word -> 1)
        var res = ListMap(count.toSeq.sortWith(_._2 > _._2): _*)
        return res      
    }

    def TF () =
      var max_count = 1
      for doc <- docs_count.keys do
        max_count = docs_count(doc).max._2
        docs_tf.addOne(doc, docs_count(doc).transform((k, v) => v/max_count))

    def TFIDF () =
      val num_of_docs = docs_count.size
      for doc <- docs_count.keys do
        docs_tfidf.addOne(doc, docs_count(doc).transform((k, v) => v*math.log10(num_of_docs/idf(k))/math.log10(2.0)))

    def printCount (wordCount: ListMap[String, Int]) = wordCount.foreach{case(key, value)=> println(key)}

    def printWords (n : Int) : Unit = 
        docs_count.foreach{case(key, value) => 
            println(key + " - " + value.dropRight(value.size-n).keys.mkString(", "))
        }

    def printTF (n : Int) : Unit = 
        docs_tf.foreach{case(key, value) => 
            println(key + " -- " + value.dropRight(value.size-n).keys.mkString(", "))
    }

    def printTFIDF (n : Int) : Unit = 
        docs_tfidf.foreach{case(key, value) => 
            println(key + " -> " + value.dropRight(value.size-n).keys.mkString(", "))
    }

@main def tfidf = 
  var d = new Input
  //var list_of_docs : ListBuffer[String] = ListBuffer("alice_in_wonderland.txt", "sherlock_holmes.txt")
  //var list_of_docs : ListBuffer[String] = ListBuffer("sh1.txt","sh2.txt", "sh3.txt", "sh4.txt", "sh5.txt", "sh6.txt", "sh7.txt", "sh8.txt", "sh9.txt", "sh10.txt" )
  //var list_of_docs : ListBuffer[String] = ListBuffer("sh1.txt","sh2.txt", "sh3.txt", "sh4.txt", "sh5.txt","astr1.txt","astr2.txt", "astr3.txt", "astr4.txt", "astr5.txt")
  var list_of_docs : ListBuffer[String] = ListBuffer("travel.txt", "opera.txt", "myths.txt", "cook.txt", "history.txt", "art.txt", "sh1.txt","astr1.txt", "alice_in_wonderland.txt", "bio.txt")
  var l = list_of_docs.size
  for doc <- list_of_docs do
    d.addFile(doc)
  println("Most frequent words from all docs: ")
  d.sumUp(10)
  d.TF()
  d.TFIDF()
  println("Simple word count: ")
  d.printWords(10)
  println("TF.IDF: ")
  d.printTFIDF(10)