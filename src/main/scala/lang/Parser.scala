package lang

import lang.Atom.*
import scala.util.parsing.combinator.*

object Parser extends RegexParsers with PackratParsers {
  type P[+T] = PackratParser[T]
  class From[T](p: Parser[T]) {
    def apply(s: String): T =
      parseAll(p, s.takeWhile(_ != '%')).getOrElse(error("parse error"))
  }
  override protected val whiteSpace = """(\s|c.*)+""".r
  private val d: String = "0-9"
  private lazy val num: P[Int] = s"-?[$d]+".r ^^ { _.toInt }

  private lazy val p: P[(Int, Int)] =
    "p cnf" ~> num ~ num ^^ { case nv ~ nc => (nv, nc) }
  private lazy val v: P[Int] = num filter (_ != 0)
  private lazy val l: P[Literal] = v ^^ { v => Literal(v > 0, Var(v.abs)) }
  private lazy val c: P[Clause] = rep1(l) <~ "0"
  private lazy val cnf: P[CNF] = rep(c)

  lazy val formatted = p ~ cnf ^^ {
    case (nv, nc) ~ cnf =>
      if (nv != cnf.flatten.map(_._2).toSet.size)
        error("Number of variables mismatch")
      if (nc != cnf.length)
        error("Number of clauses mismatch")
      cnf
  }

  /** Helper utilities */
  private def error(): Nothing = error("")
  private def error(msg: String): Nothing = throw Exception(msg)
}

object DIMACS extends Parser.From(Parser.formatted)
