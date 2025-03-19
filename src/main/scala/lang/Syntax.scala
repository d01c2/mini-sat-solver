package lang

// NOTE: Inputs are guaranteed to be CNF; general formula handling not required.

enum Atom:
  case Symbol(b: Boolean)
  case Var(x: Int)

case class Literal(sign: Boolean, atom: Atom)

type Clause = List[Literal]
type CNF = List[Clause]
