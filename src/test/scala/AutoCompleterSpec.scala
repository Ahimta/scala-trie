import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalacheck.{Gen, Arbitrary, Properties}

import scala.util.Random

/**
 * Created by Abdullah Alansari on 3/24/15.
 */
object AutoCompleterSpec extends Properties("AutoCompleter") {


  /**
   *
   * @param size maximum size of the returned string
   * @return non-empty lower-cased random string
   */
  private def randomString(size: Int = 17): String = {

    val length = Random.nextInt(size) + 1

    val chars = new Array[Char](length)

    for (i <- 0 until length) {
      val intChar = Random.nextInt(27)
      val char    = if (intChar == 26) { '\'' } else { (intChar + 97).toChar }
      chars(i) = char
    }

    String.valueOf(chars)
  }

  /**
   *
   * @param ops number of operations performed on the generated auto-completer
   * @return random auto-completer
   */
  private def randomCompleter(ops: Int): AutoCompleter = {

    val completer = new AutoCompleter()

    for (_ <- 1 to ops) {

      Random.nextInt(8) match {

        case 0 => completer.autoComplete(randomString())
        case 1 => completer.exists(randomString())
        case 2 => completer.count(randomString())
        case 3 => completer.add(randomString())
        case 4 => completer.isEmpty
        case 5 => completer.sorted
        case 6 => completer.levels
        case 7 => completer.size
      }
    }

    completer
  }

  implicit val randomCompleter: Arbitrary[AutoCompleter] = Arbitrary(Gen.sized(randomCompleter(_)))

  property("test") = forAll { (completer: AutoCompleter) =>

    val prefix = randomString()
    val levels = completer.levels
    val size   = completer.size

    completer.add(prefix)

    val prefixed = completer.autoComplete(prefix)
    val sortedSize = completer.sorted.size

    (sortedSize == size || sortedSize == size + 1) :| "sorted"            &&
    (completer.count(prefix) >= 1)                 :| "prefix count"      &&
    (prefixed.size <= (size + 1))                  :| "autocomplete size" &&
    (completer.levels >= levels)                   :| "levels"            &&
    (completer.size   >= size)                     :| "size"              &&
    prefixed.contains(prefix)                      :| "autocomplete"      &&
    completer.exists(prefix)                       :| "prefix"            &&
    !completer.isEmpty                             :| "isEmpty"
  }
}
