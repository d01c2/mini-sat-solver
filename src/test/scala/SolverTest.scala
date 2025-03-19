import lang.*
import solver.*
import Satisfiability.*

class SolverTest extends munit.FunSuite {
  test("should solve satisfiable OR of AND clauses") {
    val input = readFormula("formula1.cnf")
    assertEquals(Solver.solve(DIMACS(input))._1, SAT)
  }

  test("should solve unsatisfiable implication with contradiction") {
    val input = readFormula("formula2.cnf")
    assertEquals(Solver.solve(DIMACS(input))._1, UNSAT)
  }

  test("should solve satisfiable shared variable formula") {
    val input = readFormula("formula3.cnf")
    assertEquals(Solver.solve(DIMACS(input))._1, SAT)
  }

  test("should solve satisfiable complex multiclause formula") {
    val input = readFormula("formula4.cnf")
    assertEquals(Solver.solve(DIMACS(input))._1, SAT)
  }

  /** Helper utilities */
  private def readFormula(file: String): String = scala.io.Source
    .fromFile(s"./src/test/scala/data/$file")
    .getLines
    .mkString("\n")
}
