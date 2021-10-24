import scala.io.Source
import scala.collection.immutable.ListMap
import java.io._

@main def word_count(n: Int) =
  val filename = "alice_in_wonderland.txt"
  val stopwords = Source.fromFile("stop_words_english.txt").getLines().toList
  val fileContents = Source
    .fromFile(filename)
    .getLines
    .flatMap(line => line.split("""\W+"""))
    .map(_.toLowerCase())
    .toList
    .filter(!stopwords.contains(_))
    .filterNot(_ == "")

  var count = scala.collection.mutable.Map[String, Int]()
  for word <- fileContents do
    if (count.contains(word)) then count(word) += 1;
    else count.addOne(word -> 1)
  //count.foreach(println)
  var res = ListMap(
    count.toSeq.sortWith(_._2 > _._2): _*
  ) //.sortWith(_._2 > _._2):_*) should work for descending
  res = if res.size > n then res.dropRight(res.size - n) else res
  res.foreach { case (key, value) => println(key) }
  //stopwords.foreach{case(v) => println(s"_${v}_")}
  val file = new File("alice.csv")
  val bw = new BufferedWriter(new FileWriter(file))
  for record <- res do
    var text = record._2.toString + ", " + record._1 + ", 000000 \n"
    bw.write(text)

  bw.close()
