package solution

import java.io.PrintWriter
import java.nio.file.{Path, Paths}

import scala.io.{Source, StdIn}

object Process {

  def readLines(path: Path): Seq[String] = {
    val source = Source.fromFile(path.toFile)
    val res = source.getLines().toList
    source.close()
    res
  }

  def writeLines(path: Path, lines: Seq[String]): Unit = {
    val pw = new PrintWriter(path.toFile)
    lines.foreach { line =>
      pw.write(line)
      pw.write("\n")
    }
    pw.close()
  }

  def findWords(lettersPath: Path, wordsPath: Path, outputPath: Path): Unit = {
    var letters: Map[String, Int] = readLines(lettersPath)
      .map { _ -> 1 }
      .toList
      .groupBy { case (letter, _) => letter }
      .mapValues { _.length }

    def has(letter: String, count: Int): Boolean =
      letters.get(letter)
        .exists(_ >= count)

    def use(letter: String, count: Int): Unit =
      letters.get(letter).foreach {
        case n if n > count =>
          letters += letter -> (n - count)
        case n if n == count =>
          letters -= letter
      }

    val words = scala.util.Random.shuffle(readLines(wordsPath))

    var result: List[String] = List.empty

    words.foreach { word =>
      val chars: Map[String, Int] = word.groupBy(c => c.toString).mapValues(_.length)

      if (chars.forall { case (letter, count) => has(letter, count) }) {
        result = word :: result
        chars.foreach { case (letter, count) => use(letter, count) }
      }
    }

    writeLines(outputPath, result)
  }

  def main(args: Array[String]): Unit = {

    def measure(path: Path): Int = {
      readLines(path).map(_.length).sum
    }

    while (true) {

      var res = 0
      List("a", "b", "c", "d").map { n =>
        val lettersPath = Paths.get("./input", s"${n}_letters.txt")
        val wordsPath = Paths.get("./input", s"${n}_words.txt")
        val outputPath = Paths.get("./solutions", s"${n}_result.txt")
        findWords(lettersPath, wordsPath, outputPath)

        res += measure(outputPath)
      }


      // This is a DP problem, but today I am lazy   (=

      println(res)
      if (res > 9800) {
        println(res)
        StdIn.readLine()
      }

    }

  }
}
