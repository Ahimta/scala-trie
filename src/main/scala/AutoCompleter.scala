import scala.annotation.tailrec
import scala.collection.mutable

/**
 * Created by Abdullah Alansari on 3/23/15.
 */
final class AutoCompleter {

  import AutoCompleter._

  private val root: Node = nextNode()
  private var _levels = 0
  private var _size   = 0

  def isEmpty: Boolean = size == 0
  def levels: Int = _levels
  def size:   Int = _size

  def sorted: mutable.SortedSet[String] = autoComplete("")

  def exists(word: String): Boolean = {

    require(isValid(word))

    @tailrec
    def loop(parent: Node, i: Int): Boolean = {

      if (i >= word.length) { false }
      else {

        val char  = word.charAt(i).toLower

        parent.next(indexForChar(char)) match {

          case None => false
          case Some(node@Node(children, isWord, _)) =>
            if (i + 1 == word.length) { isWord            }
            else                      { loop(node, i + 1) }
        }
      }
    }

    loop(root, 0)
  }

  def add(word: String): Unit = {

    require(isValid(word), word)

    @tailrec
    def loop(parent: Node, i: Int): Unit = {

      if (i < word.length) {

        _levels = Math.max(levels, i + 1)

        val children  = parent.next
        val char      = word.charAt(i).toLower
        val charIndex = indexForChar(char)

        val isLastChar = i == word.length - 1

        children(charIndex) match {

          case Some(node) =>
            if (isLastChar && !node.isWord) { _size += 1 }
            node.isWord ||= isLastChar
            node.count   += 1
            loop(node, i + 1)

          case None =>
            if (isLastChar) { _size += 1 }
            val node = nextNode(isWord = isLastChar)
            children(charIndex) = Some(node)
            loop(node, i + 1)
        }
      }
    }

    loop(root, 0)
  }

  def count(prefix: String): Int = {

    require(isValid(prefix))

    @tailrec
    def loop(parent: Node, i: Int): Int = {

      if (i >= prefix.length) { 0 }
      else {

        parent.next(indexForChar(prefix.charAt(i))) match {

          case None => 0
          case Some(node@Node(_, _, count)) =>
            if (i + 1 == prefix.length) { count             }
            else                        { loop(node, i + 1) }
        }
      }
    }

    loop(root, 0)
  }

  def autoComplete(prefix: String): mutable.SortedSet[String] = {

    require(isValid(prefix))

    val set: mutable.SortedSet[String] = mutable.SortedSet()

    def helper(node: Option[Node], letters: List[Char], i: Int, charIndex: Int): Unit = node match {

      case None =>
      case Some(node@Node(_, isWord, _)) =>

        val char = charForIndex(charIndex)
        val reversedLetters = char::letters
        if (isWord) { set.add(reversedLetters.reverse.mkString) }
        loop(node, reversedLetters, i + 1)
    }

    def loop(parent: Node, letters: List[Char], i: Int): Unit = {

      val children = parent.next

      if (i < prefix.length) {
        val char      = prefix.charAt(i).toLower
        val charIndex = indexForChar(char)
        helper(children(charIndex), letters, i, charIndex)
      }
      else {
        for (charIndex <- MIN_INDEX to MAX_INDEX) { helper(children(charIndex), letters, i, charIndex) }
      }
    }

    loop(root, List(), 0)
    set
  }

  // Expensive operation used only for debugging
  override def toString: String = {

    val builder = new StringBuilder(levels * (MAX_INDEX + 6))
    val grid    = getGrid

    for (i <- 0 until grid.length) {

      val level = i + 1
      val row   = grid(i)

      builder
        .append("%03d:".format(level))
        .append(String.valueOf(row))
        .append('\n')
    }

    builder.toString()
  }

  // Expensive operation used only for debugging
  private def getGrid: Array[Array[Char]] = {

    val grid = new Array[Array[Char]](levels)

    for (i <- 0 until levels) { grid(i) = new Array[Char](MAX_INDEX + 1) }

    def loop(node: Node, level: Int): Unit = {

      val y = level + 1

      if (y < levels) {
        
        val row = grid(y)

        for (x <- 0 to MAX_INDEX) {

          node.next(x) match {
            case None => if (row(x) == '\0') { row(x) = '-' }
            case Some(child) =>
              row(x) = charForIndex(x)
              loop(child, level + 1)
          }
        }
      }
    }

    loop(root, -1)
    grid
  }
}


object AutoCompleter {

  val APOSTROPHE_INDEX = 26
  val APOSTROPHE       = '\''

  val FIRST_INDEX = 97
  val LAST_INDEX  = 122
  val MIN_INDEX   = 0
  val MAX_INDEX   = 26

  protected final case class Node(next: Array[Option[Node]], var isWord: Boolean = false, var count: Int = 1)

  def isValid(s: String): Boolean = s.forall { char =>

    char == APOSTROPHE || char.toLower.toInt >= FIRST_INDEX && char.toLower.toInt <= LAST_INDEX
  }

  private def nextNode(isWord: Boolean = false): Node = {

    val xs = new Array[Option[Node]](MAX_INDEX + 1)
    for (i <- MIN_INDEX to MAX_INDEX) { xs(i) = None }
    Node(xs, isWord)
  }

  private def charForIndex(i: Int): Char = {

    require(i >= MIN_INDEX && i <= MAX_INDEX)

    if (i == APOSTROPHE_INDEX) { APOSTROPHE               }
    else                       { (FIRST_INDEX + i).toChar }
  }

  private def indexForChar(char: Char): Int = {

    val c = char.toLower
    val i = c.toInt

    require(c == APOSTROPHE || i >= FIRST_INDEX && i <= LAST_INDEX)

    if (c == APOSTROPHE) { APOSTROPHE_INDEX }
    else                 { i - FIRST_INDEX  }
  }
}
