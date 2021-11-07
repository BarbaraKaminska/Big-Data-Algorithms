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
    var docs_tfidf = Map[String, Map[String, Double]]() // tf.idf of words grouped by docs
    var docs_tf = Map[String, Map[String, Float]]() // tf of words grouped by docs
    var docs_count = Map [String, ListMap[String, Int]]() // count of words grouped by docs
    var idf = Map[String, Int]() //number of docs in which word appears 

    def addFile (filename:String) = 
        getFromFile(filename)
        removeStopwords()
        docs_count.addOne(filename, countWord())        
        clearTemp()

    def sumUp () = 
        var res = ListMap(contents.toSeq.sortWith(_._2 > _._2): _*)
        docs_count.addOne("all", res)

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
/*
    def count_tf (m : Map[], max_count : Int) : Unit = {
      m.transform((key,value) => value/max_count)
    }
*/
    def printCount (wordCount: ListMap[String, Int]) = wordCount.foreach{case(key, value)=> println(key)}

    def printMostFreq (n : Int) : Unit = 
        docs_count.foreach{case(key, value) => 
            println(key + " - " + value.dropRight(value.size-n).keys.mkString(", "))
        }

@main def tfidf = 
  var d = new Input
  var max_count = 0
  var list_of_docs : ListBuffer[String] = ListBuffer("alice_in_wonderland.txt", "stud.txt")
  var l = list_of_docs.size
  for doc <- list_of_docs do
    d.addFile(doc)
  d.sumUp()
  d.printMostFreq(10)