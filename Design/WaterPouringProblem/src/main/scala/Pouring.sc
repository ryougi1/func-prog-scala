/**
 * Class pouring for solving the water pouring problem
 *
 * @param capacity is the vector of all the glasses and their respective
 *                 capacities i.e. Vector(4, 9), glass 1 has cap 4, glass 2
 *                 has cap 9
 */
class Pouring(capacity: Vector[Int]) {


  /**
   * States are represented as the vector of all glasses and which level their
   * are filled to respectively. E.g. Vector(0, 9), glass one is empty, glass
   * two is filled to 9.
   * Initially, all glasses are empty.
   */
  type State = Vector[Int]
  val initialState = capacity map (x => 0)

  /**
   * All moves change the state.
   * Parameters glass, from, to are the indexes of the glasses involved, from
   * the state vector
   */
  trait Move {
    def change(state: State): State
  }

  case class Empty(glass: Int) extends Move {
    def change(state: State) = state updated(glass, 0)
  }

  case class Fill(glass: Int) extends Move {
    def change(state: State) = state updated(glass, capacity(glass))
  }

  case class Pour(from: Int, to: Int) extends Move {
    def change(state: State) = {
      val amount = state(from) min (capacity(to) - state(to))
      state updated(from, state(from) - amount) updated(to, state(to) + amount)
    }
  }

  /**
   * Helper variable glasses
   * Moves is the vector of all legal moves, given the available glasses
   */
  val glasses = 0 until capacity.length
  val moves = (for (g <- glasses) yield Empty(g)) ++
    (for (g <- glasses) yield Fill(g)) ++
    (for (from <- glasses; to <- glasses if from != to)
      yield Pour(from, to))

  /**
   * A path is a sequence of moves that hopefully lead to the desired outcome.
   * History is taken in reverse, so last move in the path comes first in that
   * history list.
   */
  class Path(history: List[Move], val endState: State) {
    //    def endState: State = trackState(history)
    //    private def trackState(xs: List[Move]): State = xs match {
    //      case Nil => initialState
    //      case move :: xs1 => move change trackState(xs1)
    //    }
    /**
     * Function does the same work as the recursive one above.
     * def foldRight(z: B)(op: (A, B) => B): B
     * Call foldRight (since first move is last in list) on history with
     * initialState as initial value z, and for every move A in history
     * apply the change to the state B.
     *
     */
    //    def endState: State = history.foldRight(initialState) { (move, state) => move.change(state) }
    //    def endState: State = (history foldRight initialState) (_ change _)

    /**
     * Instead of doing a foldRight every time an endstate is to needed,
     * store the endstate in the path to avoid recomputation.
     * Extend then needs to apply the latest move on the previous endstate to
     * keep endstate current.
     */
    def extend(move: Move): Path =
      new Path(move :: history, move change endState)

    override def toString: String =
      (history.reverse mkString " ") + "--> " + endState
  }

  val initialPath = new Path(Nil, initialState)

  /**
   * For every path in paths, apply all possible moves.
   * For the resulting paths, check that their endstate is not already an
   * explored endstate.
   * For all remaining paths, add to set of paths, and set of already explored
   * paths.
   * Use Set to avoid duplicates, use Stream to evaluate to infinity without
   * actually evaluating.
   */
  def from(paths: Set[Path], explored: Set[State]): Stream[Set[Path]] =
    if (paths.isEmpty) Stream.empty
    else {
      val more = for {
        path <- paths
        next <- moves map path.extend
        if !(explored contains next.endState)
      } yield next
      paths #:: from(more, explored ++ (more map (_.endState)))
    }

  val pathSets = from(Set(initialPath), Set(initialState))

  /**
   * Since all paths are already Stream generated, just need to extract the
   * paths and check whether the glasses in the endstate contain the target.
   */
  def solutions(target: Int): Stream[Path] =
    for {
      pathSet <- pathSets
      path <- pathSet
      if (path.endState contains target)
    } yield path

}

val problem = new Pouring(Vector(4, 9))
problem.moves
//problem.pathSets.take(3).toList
problem.solutions(6)
