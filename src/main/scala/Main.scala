import java.io.{IOException, FileNotFoundException}

import scala.io.Source

/**
 * Created by Abdullah Alansari on 3/23/15.
 */
object Main {

  def main(args: Array[String]): Unit = {

    val path = args.headOption.getOrElse("/usr/share/dict/words")

    try {
      val autoCompleter = new AutoCompleter
      val file = Source.fromFile(path)

      for (word <- file.getLines() if AutoCompleter.isValid(word)) { autoCompleter.add(word) }
      for (word <- autoCompleter.sorted) { if (!autoCompleter.exists(word)) println(false) }
      println(autoCompleter)
    }
    catch {
      case _: FileNotFoundException => println(s"$path doesn't exist.")
      case _: IOException           => println(s"Unknown error: $path")
    }
  }
}
