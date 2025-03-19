import lang.*
import solver.*
import java.io.File
import scala.io.Source
import scala.util.{Try, Success, Failure}

// NOTE: This code is just a driver code with UI.
// The actual implementations are inside lang and solver packages.

case class FormulaStats(satCount: Int = 0, total: Int = 0):
  def unsatCount = total - satCount
  def satPercentage = if total > 0 then satCount.toDouble / total * 100 else 0
  def unsatPercentage =
    if total > 0 then unsatCount.toDouble / total * 100 else 0
  def isAllSat = total > 0 && satCount == total
  def isAllUnsat = total > 0 && unsatCount == total
  def add(isSat: Boolean) =
    copy(satCount = if isSat then satCount + 1 else satCount, total = total + 1)

object UI:
  def info(msg: String) = println(s"[info] $msg")
  def warn(msg: String) = println(s"[warning] $msg")
  def error(msg: String) = print(s"\u001B[s\u001B[999;1H\u001B[K$msg\u001B[u")

  object Progress:
    private val width = 40
    private val spinner = Array('|', '/', '-', '\\')

    def show(
      line: Int,
      current: Int,
      total: Int,
      category: String,
      file: String,
    ): Unit =
      val percent = if total > 0 then current * 100 / total else 0
      val filled = if total > 0 then current * width / total else 0
      val bar =
        "[" + "=".repeat(filled) + (if filled < width then ">" else "") +
        " ".repeat(
          Math.max(0, width - filled - (if filled < width then 1 else 0)),
        ) + "]"
      val label = category.padTo(9, ' ')

      print(s"\u001B[${line};1H\u001B[K$label: ${spinner(
          current % spinner.length,
        )} $bar $percent% ($current/$total) $file")

    def setup(lines: Int): Unit =
      for _ <- 1 to lines do println()
      print(s"\u001B[${lines}A")

    def finish(lines: Int): Unit =
      print(s"\u001B[${lines}B\u001B[J\r")

  object Dashboard:
    private val check = "\u2714"
    private val cross = "\u2718"
    private val borders = (
      "╔════════════════════════ BENCHMARK RESULTS ════════════════════════╗",
      "╟────────────┬──────────┬────────────┬──────────────┬───────────────╢",
      "╟────────────┼──────────┼────────────┼──────────────┼───────────────╢",
      "╚════════════╧══════════╧════════════╧══════════════╧═══════════════╝",
    )

    def show(
      results: Seq[(String, Satisfiability, Option[Assignment], SolverStats)],
    ): Unit =
      val (header, sep1, sep2, footer) = borders
      val (ufStats, uufStats) =
        results.foldLeft((FormulaStats(), FormulaStats())) {
          case ((uf, uuf), (name, sat, _, _)) =>
            if name.startsWith("uf") then
              (uf.add(sat == Satisfiability.SAT), uuf)
            else if name.startsWith("uuf") then
              (uf, uuf.add(sat == Satisfiability.SAT))
            else (uf, uuf)
        }
      val byCategory = results.groupBy {
        case (name, _, _, _) =>
          if name.startsWith("uf20") then "uf20"
          else if name.startsWith("uf50") then "uf50"
          else if name.startsWith("uuf50") then "uuf50"
          else "other"
      }
      println(header)
      val ufMark = if ufStats.isAllSat then check else cross
      val uufMark = if uufStats.isAllUnsat then check else cross
      println(
        f"║ $ufMark SAT   formulas correctly solved: ${ufStats.satCount}/${ufStats.total}%-4d (${ufStats.satPercentage}%6.2f%%)            ║",
      )
      println(
        f"║ $uufMark UNSAT formulas correctly solved: ${uufStats.unsatCount}/${uufStats.total}%-4d (${uufStats.unsatPercentage}%6.2f%%)            ║",
      )
      println(sep1)
      println(
        "║ Category   │ Count    │ Decisions  │ Propagations │ Prop/Decision ║",
      )
      println(sep2)
      var totals = (0, 0, 0, 0, 0)
      byCategory.toList.sortBy(_._1).foreach {
        case (category, formulas) =>
          val count = formulas.size
          val decisions = formulas.map(_._4.decisions).sum
          val propagations = formulas.map(_._4.propagations).sum
          val ratio =
            if decisions > 0 then propagations.toDouble / decisions else 0.0

          val satViolations = formulas.count {
            case (name, sat, _, _) =>
              name.startsWith("uf") && sat != Satisfiability.SAT
          }
          val unsatViolations = formulas.count {
            case (name, sat, _, _) =>
              name.startsWith("uuf") && sat != Satisfiability.UNSAT
          }

          totals = (
            totals._1 + decisions,
            totals._2 + propagations,
            totals._3 + count,
            totals._4 + satViolations,
            totals._5 + unsatViolations,
          )

          println(
            f"║ ${category}%-10s │ ${count}%-8d │ ${decisions}%-8d   │ ${propagations}%-10d   │  ${ratio}%11.2f  ║",
          )
      }

      val (
        totalDecisions,
        totalProps,
        totalCount,
        satViolations,
        unsatViolations,
      ) = totals
      val totalRatio =
        if totalDecisions > 0 then totalProps.toDouble / totalDecisions else 0.0

      println(sep2)
      println(
        f"║ TOTAL      │ ${totalCount}%-8d │ ${totalDecisions}%-8d   │ ${totalProps}%-10d   │  ${totalRatio}%11.2f  ║",
      )
      println(footer)
      println()
      println()

      if satViolations > 0 || unsatViolations > 0 then
        println("CORRECTNESS VIOLATIONS:")
        if satViolations > 0 then
          println(f" Expected SAT, Actually UNSAT: $satViolations")
        if unsatViolations > 0 then
          println(f" Expected UNSAT, Actually SAT: $unsatViolations")
        println("─" * 74)

def processFile(
  file: File,
  resultDir: String,
): (String, Satisfiability, Option[Assignment], SolverStats) =
  val fileName = file.getName
  Try {
    val formula = Source.fromFile(file).getLines().mkString("\n")
    val result = Solver.solve(DIMACS(formula))

    val baseFileName = fileName.stripSuffix(".cnf")
    val outputFile = new File(s"$resultDir/$baseFileName.result")
    outputFile.getParentFile.mkdirs()

    val writer = new java.io.PrintWriter(outputFile)
    result match
      case (Satisfiability.SAT, Some(assignment), _) =>
        val sortedVars = assignment.toList
          .sortBy(_._1.x)
          .map { case (v, b) => s"${if b then "" else "-"}${v.x}" }
          .mkString(" ") + " 0"
        writer.println(sortedVars)
      case _ => writer.println("UNSAT")
    writer.close()

    (fileName, result._1, result._2, result._3)
  } match
    case Success(res) => res
    case Failure(err) =>
      UI.error(s"ERROR: ${err.getMessage}")
      (fileName, Satisfiability.UNSAT, None, SolverStats())

def findFiles(path: String): List[File] =
  val dir = new File(path)
  if !dir.exists || !dir.isDirectory then List.empty
  else
    val files = Option(dir.listFiles).getOrElse(Array.empty[File]).toList
    files ++ files.filter(_.isDirectory).flatMap(d => findFiles(d.getPath))

@main def main: Unit =
  val resultDir = "./datasets/results"
  new File(resultDir).mkdirs()
  val cnfs =
    findFiles("./datasets").filter(f => f.isFile && f.getName.endsWith(".cnf"))
  if cnfs.isEmpty then
    UI.warn("No *.cnf files found in dataset")
    return

  val byCategory = cnfs
    .groupBy { file =>
      val name = file.getName
      if name.startsWith("uf20") then "SAT-20"
      else if name.startsWith("uf50") then "SAT-50"
      else if name.startsWith("uuf50") then "UNSAT-50"
      else "Other"
    }
    .view
    .mapValues(_.toList)
    .toMap
  val categories = byCategory.keys.toSeq.sorted

  UI.info(s"Found ${cnfs.length} *.cnf files to process:")
  categories.foreach(c =>
    println(s"       - $c: ${byCategory(c).length} files"),
  )
  UI.info("Processing files...")
  UI.Progress.setup(categories.length)

  var allResults =
    List.empty[(String, Satisfiability, Option[Assignment], SolverStats)]

  for ((category, idx) <- categories.zipWithIndex) do
    val files = byCategory(category).sortBy(_.getName)
    for ((file, fileIdx) <- files.zipWithIndex) do
      UI.Progress.show(
        idx + 1,
        fileIdx + 1,
        files.length,
        category,
        file.getName,
      )
      val result = processFile(file, resultDir)
      allResults = result :: allResults

  val results = allResults.reverse
  UI.Progress.finish(categories.length)
  UI.Dashboard.show(results)
