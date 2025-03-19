package solver

import lang.*
import lang.Atom.*
import scala.collection.mutable.{Map => MMap}

enum Satisfiability:
  case SAT, UNSAT

type Assignment = MMap[Var, Boolean]

case class SolverStats(decisions: Int = 0, propagations: Int = 0)

object Solver:
  private var stats = SolverStats()

  def solve(formula: CNF): (Satisfiability, Option[Assignment], SolverStats) =
    env.clear()
    stats = SolverStats()
    if dpll(formula) then (Satisfiability.SAT, Some(env.clone()), stats)
    else (Satisfiability.UNSAT, None, stats)

  private val env: Assignment = MMap.empty

  /** Boolean Constraint Propagation (BCP) */
  private def propagate(formula: CNF): CNF =
    formula.find(_.isUnit) match
      case Some(unitClause) =>
        val unitLiteral = unitClause.head
        unitLiteral.atom match
          case Symbol(_) => formula
          case v: Var =>
            stats = stats.copy(propagations = stats.propagations + 1)
            propagate(formula.substitute(v -> Symbol(unitLiteral.sign), env))
      case None => formula

  /** Optimization: Pure Literal Elimination (PLE) */
  private def optimize(formula: CNF): CNF =
    val pureLiterals = for {
      case (_: Var, literals) <- formula.flatten.groupBy(_.atom)
      signs = literals.map(_.sign).distinct
      if signs.length == 1
      pureLit = literals.head
    } yield pureLit
    if (pureLiterals.nonEmpty)
      stats = stats.copy(propagations = stats.propagations + pureLiterals.size)
    pureLiterals.foldLeft(formula) {
      case (acc, Literal(sign, v: Var)) =>
        acc.substitute(v -> Symbol(sign), env)
      case (acc, _) => acc
    }

  private def choose(variables: List[Var]): Var = variables.head

  private def dpll(formula: CNF): Boolean =
    val simplified = (propagate andThen optimize)(formula)
    if simplified.isEmpty then true
    else if simplified.exists(_.isEmpty) then false
    else
      stats = stats.copy(decisions = stats.decisions + 1)
      val p = choose(simplified.variables)
      dpll(simplified.substitute(p -> Symbol(false), env)) ||
      dpll(simplified.substitute(p -> Symbol(true), env))

extension (literal: Literal)
  private def isFalsy: Boolean = literal match
    case Literal(sign, Symbol(b)) => if sign then !b else b
    case _                        => false

  private def isTruthy: Boolean = literal match
    case Literal(sign, Symbol(b)) => if sign then b else !b
    case _                        => false

  private def negate: Literal = literal.atom match
    case Symbol(b)  => Literal(true, Symbol(!b))
    case v @ Var(_) => Literal(!literal.sign, v)

extension (clause: Clause) def isUnit: Boolean = clause.length == 1

extension (formula: CNF)
  private def substitute(mapping: (Var, Symbol), env: Assignment): CNF =
    val x -> Symbol(b) = mapping
    env(x) = b
    val substituted = for {
      clause <- formula
      substitutedClause = for {
        literal <- clause
        Literal(sign, atom) = literal
        newLiteral = atom match
          case Symbol(b)            => if b then literal else literal.negate
          case v @ Var(_) if v == x => Literal(sign, Symbol(b))
          case _                    => literal
      } yield newLiteral
    } yield substitutedClause
    substituted.simplify

  private def simplify: CNF =
    val simplified = for {
      clause <- formula
      nonFalsy = clause.filterNot(_.isFalsy)
      if !nonFalsy.exists(_.isTruthy)
    } yield nonFalsy
    if simplified.exists(_.isEmpty) then List(List()) else simplified

  private def variables: List[Var] = formula.flatten
    .collect { case Literal(_, v: Var) => v }
    .groupBy(identity)
    .toList
    .sortBy(-_._2.size)
    .map(_._1)
