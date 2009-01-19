/*
 * A Scala example for file IO with some functional niceness. Some things were
 * tricky to figure out, thanks to #scala for answering my questions and
 * solving my worst problems.
 *
 * 2009 by Marek Kubica <marek@xivilization.net>
 */

import java.io.FileWriter
import scala.io.Source

/* a custom class with some handy string methods known from scripting
 * languages like Python */
class ScriptingString(original: String) {
  /* default rtrim, considers space, \n \r and \t as whitespace.
   * trims whitespace from right side of string */
  def rtrim: String = rtrim(" \n\r\t")
  /* adjustable rtrim */
  def rtrim(cutout: String): String = {
    // crazy functional solution. Terrible performance but nice dropWhile
    original.reverse.toArray.dropWhile (char => (cutout.contains(char))).reverse.mkString
  }
}

/* Implicit conversion definitions
 * See some documentation about implicit conversions here:
 * <http://scalada.blogspot.com/2008/03/implicit-conversions-magical-and.html>
 */
object ImplicitScripting {
  implicit def string2scripting(x: String) = new ScriptingString(x)
}

// implicit conversions are scoped, apply them in this file
import ImplicitScripting._

object Process {
  def main(args: Array[String]) {
    // Scala IO library can currently only read, we need Java to write
    // They accept contributions, though
    val src = Source.fromFile("Process.scala")
    val dest = new FileWriter("Processed.scala")

    // oh look, a closure. Sweet!
    def process(line: String) {
      val clean_string = line.rtrim
      if (clean_string contains "{") {
        dest.write(clean_string.substring(0, 5) + "\n")
      }
    }
    // apply the closure on every line
    src.getLines foreach process
    dest close
  }
}
