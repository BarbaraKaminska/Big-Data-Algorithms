import scala.io.Source
import scala.io.StdIn.readLine
import scala.collection.mutable.Map
import scala.collection.immutable.ListMap
import scala.collection.mutable.ListBuffer
import scala.util.control.Exception._
import util.control.Breaks._
import java.io._

class Input:
    val stopwords = Source.fromFile("stop_words_english.txt").getLines.toList
    var contents: ListBuffer[String] = ListBuffer()
    
    def getFromFile(filename: String) = 
      contents.addAll(Source.fromFile(filename).getLines.flatMap(line => line.split("""\W+""")).toArray)

    def getFromStd = 
      contents.addAll(readLine().split("""\W+""").toArray)

    def removeStopwords(contents: ListBuffer[String]) = 
    contents
    .map(_.toLowerCase())
    .toList
    .filter(! stopwords.contains(_))
    .filterNot(_ == "")

    def mostFrequentWord (contents: ListBuffer[String], n: Int) : ListMap[String, Int] = {
        var count = scala.collection.mutable.Map[String, Int]()
        var words = removeStopwords(contents)
        for word <- words do
            if (count.contains(word)) then count(word) += 1;
            else count.addOne(word -> 1)
        //count.foreach(println)
        var res = ListMap(count.toSeq.sortWith(_._2 > _._2): _*)
        //.sortWith(_._2 > _._2):_*) should work for descending

        if (n < res.size)
          res.dropRight(res.size - n)
        else res
        
    }

    def printCount (wordCount: ListMap[String, Int]) = wordCount.foreach{case(key, value)=> println(key)}

    def writeToFile (wordCount: ListMap[String, Int], filepath: String) = 
        val file = new File(filepath)
        val bw = new BufferedWriter(new FileWriter(file))
        for record <- wordCount do
            var text = record._1 + ", " + record._2.toString + '\n'
            bw.write(text)
        
        bw.close()      



@main def generator = 
  var WordCloud = new Input
  var control = true
  while control do 
    println("Choose input. File - 'f', standard input - 's'. Press any key to get output.")
    var command = readLine()
    command match 
      case "f" => println("Enter filepath")
        var filepath = readLine()
        try
            WordCloud.getFromFile(filepath)
        catch
            case i: IOException => println("Invalid file")
      case "s" => println("Enter words: ")
          WordCloud.getFromStd
      case _ => control = false


  println("How many words do you want?")
  var n: Int = 30
  try
    n = readLine().toInt
  catch
    case e: NumberFormatException =>println("Invalid input. Default number of words is 30")
  var wordCount = WordCloud.mostFrequentWord(WordCloud.contents, n)
  println("Choose output: 'f' - file, 'c' - console")
  var outType = readLine()

  outType match 
    case "f" => println("Enter filepath: ")
        var outFilepath = readLine()
        try
          WordCloud.writeToFile(wordCount,  outFilepath)    
        catch
            case i: IOException => println("Invalid file")
    case "c" => WordCloud.printCount(wordCount)
    case _ => println("Invalid command")