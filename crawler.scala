import scala.io.Source
import scala.io.StdIn.readLine
import scala.collection.mutable.Map
import scala.collection.immutable.ListMap
import scala.collection.mutable.ListBuffer
import scala.util.control.Exception._
import util.control.Breaks._
import scala.util.matching.Regex
import scala.collection.mutable.LinkedHashMap
import java.io._
import scala.collection.mutable
import scala.util.Random
import scala.util.control._


class Spider: 
    
    var visitedPages = ListBuffer[String]()
    var listOfLinks = mutable.Map[String, Array[String]]() // sets of links from page
    var pageContents = mutable.Map[String, Set[String]]()

    var allPages = ListBuffer[String]()
    var allLinks = mutable.Map[String, Array[String]]() // sets of links from page
    var crawledLinks = mutable.Map[String, Array[String]]()
    var crawledContents = mutable.Map[String, Array[String]]()

    def pageContent(url : String) : Set[String] = {
        var content = Source.fromURL(url, "UTF-8")
        .mkString.replaceAll("(?s)<[^>]*>(\\s*<[^>]*>)*", " ").split("""\W+""").toSet //("<[^>]+>", " ")//
        var name = "wiki\\/[^\"]*".r.findAllIn(url).next   
        content  
    }

    def getNextLink(url : String) : String = {
        var content = Source.fromURL(url, "UTF-8").mkString
        var fullContent = content.mkString.replaceAll("(?s)<[^>]*>(\\s*<[^>]*>)*", " ").split("""\W+""").toSet
        var name = "wiki\\/[^\"]*".r.findAllIn(url).next
        var wikiPattern = "wiki\\/(?!.*%|Category|Help|Wikipedia|Special|" + 
        "Talk|Privacy_policy|Cookie_statement|Terms_of_Use|Portal|Main_page|Main_Page|File|Template)[^\"]*"
        var refs = wikiPattern.r.findAllIn(content).toSet.toArray 
        if (!listOfLinks.contains(name)) {
            listOfLinks.addOne(name, refs)
            pageContents.addOne(name, fullContent)
            visitedPages+=name
            var fw = new FileWriter("crawled_pages.txt", true) ; 
            fw.write(name + "\t " + refs.mkString(", ") + '\n') ; 
            fw.close()
            var fw1 = new FileWriter("pages_contents.txt", true) ; 
            fw1.write(name + "\t " + fullContent.mkString(", ") + '\n') ; 
            fw1.close()
        }
        val rand = new scala.util.Random
        var rnd = rand.nextInt(refs.length)
        //refs.foreach(println)

        "https://en.wikipedia.org/" + refs(rnd)
        
    }

    def crawlPages (initURL: String, n: Int) : Unit ={
        var url = initURL
        var i = 0
        while (visitedPages.length < n){
            println(i.toString)
            var prev_url = url
            try { url = getNextLink(url)
               // if (!visitedPages.contains(url)){i+=1}
            }
            catch {
                case e: FileNotFoundException => url=prev_url
            }
            i+=1
        }
        println("i: " + i.toString)
    }

    def readData (linksData : String, contentData : String) : Unit ={
        allLinks.clear()
        allPages.clear()
        crawledLinks.clear()
        crawledContents.clear()
        var i = 0
        for (line <- Source.fromFile(linksData).getLines) {
            var l = line.split("\t")
            var page = l(0)
            var links = l(1).split(", ")
            allPages += page
            allLinks.addOne(page, links)
            i+=1
        }
        //println(i)
        for (line <- Source.fromFile(contentData).getLines) {
            var l = line.split("\t")
            var page = l(0)
            var cont = l(1).split(", ")
            crawledContents.addOne(page, cont)
        }

        for ((k,v) <- allLinks){
            var visited = v.filter(x => allLinks.keySet.contains(x))
            //visited.foreach(print)
            crawledLinks.addOne(k, visited.toArray)
        }

        
    }

    def analysis() : Unit = {
        allLinks.clear()
        allPages.clear()
        crawledLinks.clear()
        crawledContents.clear()
        readData("crawled_pages.txt", "pages_contents.txt")       
        //var mapped = allLinks.map(x => x(1).map((_, x(0)))).flatten
        var mapped = crawledLinks.map(x => x(1).map((_, x(0)))).flatten
        var res = ListBuffer[(String, List[String])]()
        var g = mapped.groupBy(_(0))
        g.foreach{case(k,v) => res.addOne(k, v.map(x =>x(1)).toList)}
        //var avgLinksPerPage = mapped.size/g.size
        var avgLinksPerPage = mapped.size.toFloat/allPages.length.toFloat
        val fw = new FileWriter("analysis.txt", true) ; 
        fw.write("Average number of links per page = "+ avgLinksPerPage.toString + '\n') ; 
        fw.close()        
        for ((k,v) <- res){
            val fw = new FileWriter("analysis.txt", true) ; 
            fw.write("inDegre=" + v.length+ "\t" + k + "\t" + v.mkString(", ") + '\n') ; 
            fw.close()
        } 
    }

    def invert () : mutable.Map[String,Array[String]] = {
        var mapped = crawledLinks.map(x => x(1).map((_, x(0)))).flatten
        var res = mutable.Map[String,Array[String]]()
        var g = mapped.groupBy(_(0))
        g.foreach{case(k,v) => res.addOne(k, v.map(x =>x(1)).toArray)} 
        res
    }

    def outDegree () : mutable.Map[String,Int] = {
        var res = mutable.Map[String,Int]()
        for ((k,v) <- crawledLinks){
            res.addOne(k, v.length)
        }
        res 
    }

    def pageRank (iter : Int, filename : String) : mutable.Map[String,Double] = {
        val n = allPages.length
        var od = outDegree()
        var invertedLinks = invert()
        var oldPR = mutable.Map[String,Double]()
        allPages.foreach{x => oldPR.addOne(x, 1/n.toDouble)}
        var PR = oldPR

        for (i <- 0 to iter) {
            for (page <- allPages) {
                PR(page) = invertedLinks(page).map(x=> oldPR(x)/od(x)).sum
            }
            oldPR = PR
        }
        val res = ListMap(PR.toSeq.sortWith(_._2 > _._2): _*)
        for ((k,v) <- res){
            val fw = new FileWriter(filename, true) ; 
            fw.write(k + " \t" + v.toString + "\n") ; 
            fw.close()
        } 
        PR
    }


    def pageRankTaxation (iter : Int, filename : String) : mutable.Map[String,Double] = {
        val n = allPages.length
        val beta = 0.85
        var invertedLinks = invert()
        var od = outDegree()
        var oldPR = mutable.Map[String,Double]()
        allPages.foreach{x => oldPR.addOne(x, 1/n.toDouble)}
        var PR = oldPR

        for (i <- 0 to iter) {
            for (page <- allPages) {
                PR(page) = (1-beta)/n + beta*invertedLinks(page).map(x=> oldPR(x)/od(x)).sum
            }
            oldPR = PR
        }
        val res = ListMap(PR.toSeq.sortWith(_._2 > _._2): _*)
        for ((k,v) <- res){
            val fw = new FileWriter(filename, true) ; 
            fw.write(k + " \t" + v.toString + "\n") ; 
            fw.close()
        } 
        PR
    }

    def noTrap () : Unit = {
        allLinks.clear()
        allPages.clear()
        crawledLinks.clear()
        readData("crawled_pages.txt", "pages_contents.txt")
        pageRank(100000, "pagerank.txt")
    }

    def dealWithTrap () : Unit ={
        allLinks.clear()
        allPages.clear()
        crawledLinks.clear()
        readData("crawled_pages_trap.txt", "pages_contents.txt")
        pageRank(100000, "pagerank_trap.txt")
        pageRankTaxation(100000, "pagerank_trap_taxation.txt")
    }

    def search (word : String) : ListBuffer[String]= {
        val filename = "pagerank.txt"
        var res = ListBuffer[String]()
        var i = 0 
        readData("crawled_pages.txt", "pages_contents.txt")
        val loop = new Breaks; 
        loop.breakable{
            for (line <- Source.fromFile(filename).getLines) {
                var l = line.split(" \t")
                if(crawledContents(l(0)).contains(word)){
                    i+=1
                    res+=l(0)
                }
                if (i >= 5){loop.break}
            }
        }
             
        if (i == 0) {
            res += "Word not found"
        }
        res.foreach(println)
        res
    }

@main def crawler = 
    var c = new Spider
    var initialURL = "https://en.wikipedia.org/wiki/Web_crawler"
    //c.crawlPages(initialURL, 100)
    //c.readData("crawled_pages.txt")
    //c.allPages.foreach(println)
    //c.analysis()
    //c.pageRank.foreach(println)
    //c.pageRank(100)
    //c.dealWithTrap()
    //c.noTrap()
    c.search("Troll")