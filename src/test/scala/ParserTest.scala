import lang.*
import lang.Atom.*

class ParserTest extends munit.FunSuite {
  test("should parse DIMACS OR of AND clauses") {
    val input = readFormula("formula1.cnf")
    val expected: CNF = List(
      List(Literal(true, Var(1)), Literal(true, Var(3))),
      List(Literal(true, Var(1)), Literal(true, Var(4))),
      List(Literal(true, Var(2)), Literal(true, Var(3))),
      List(Literal(true, Var(2)), Literal(true, Var(4))),
    )
    assertEquals(DIMACS(input), expected)
  }

  test("should parse DIMACS unsatisfiable implication with contradiction") {
    val input = readFormula("formula2.cnf")
    val expected: CNF = List(
      List(Literal(false, Var(1)), Literal(true, Var(2))),
      List(Literal(true, Var(1))),
      List(Literal(false, Var(2))),
    )
    assertEquals(DIMACS(input), expected)
  }

  test("should parse DIMACS OR with shared variable") {
    val input = readFormula("formula3.cnf")
    val expected: CNF = List(
      List(Literal(true, Var(3)), Literal(true, Var(1))),
      List(Literal(true, Var(3)), Literal(true, Var(2))),
    )
    assertEquals(DIMACS(input), expected)
  }

  test("should parse DIMACS complex clause set with multiple literals") {
    val input = readFormula("formula4.cnf")
    val expected: CNF = List(
      List(
        Literal(false, Var(1)),
        Literal(false, Var(2)),
        Literal(false, Var(3)),
      ),
      List(
        Literal(true, Var(1)),
        Literal(false, Var(2)),
        Literal(true, Var(3)),
      ),
      List(
        Literal(true, Var(1)),
        Literal(true, Var(2)),
        Literal(false, Var(3)),
      ),
      List(
        Literal(true, Var(1)),
        Literal(false, Var(2)),
        Literal(false, Var(3)),
      ),
      List(Literal(false, Var(1)), Literal(true, Var(2)), Literal(true, Var(3))),
    )
    assertEquals(DIMACS(input), expected)
  }

  /** Helper utilities */
  private def readFormula(file: String): String = scala.io.Source
    .fromFile(s"./src/test/scala/data/$file")
    .getLines
    .mkString("\n")
}
