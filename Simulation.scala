import processing.core.PApplet
import scala.collection.mutable.{Set => MSet, Map => MMap}
import scala.math.exp
import scala.util.Random

package physarum {
  /** This simulation has plenty of tunable parameters, which we store here */
  object Constants {
    val decay_rate = 0.9
    val cell_permeability = 0.8
  }

  /** Trait representing pheromones, objects which can be associated with
    * a concentration */
  trait Pheromone

  /** Case object for the attractant pheromone */
  case object Attract extends Pheromone

  /** Objects which periodically emit some pheromone. */
  trait Odiferous {
    def scent: Map[Pheromone, Double]
  }

  /** A food source, represented as an object which secretes attractant */
  class Food(amt: Double) extends Odiferous {
    val amount = amt
    def scent: Map[Pheromone, Double] = Map(Attract -> amt)
  }

  /** Plasmodium, which secretes attractant */
  class Plasmodium(amount: Double) extends Odiferous {
    def scent: Map[Pheromone, Double] = Map(Attract -> amount)
  }

  /**
    * The state of an instance of [[physarum.Simulation]] is made up of a 2D
    * grid of these cells. The cell is a kind of container representing
    * the state of some discrete chunk of 2D space. It stores the pheromone
    * amounts in the space as well as food sources and plasmodium.
    */
  class Cell {
    /** Mutable set of objects at this location */
    val objects: MSet[Odiferous] = MSet()
    
    /** Mutable map of pheromone levels */
    val pheromones: MMap[Pheromone, Double] = MMap()

    /** We need this while we calculate the new pheromone levels during
      * dissipation. Since each cell's pheromone levels on the nth iteration
      * affect its neighbors' pheromone levels on the (n+1)th iteration, and
      * since we want to update pheromone levels in-place, we have to store
      * the current levels while we update the next levels. */
    val current_pheromones: MMap[Pheromone, Double] = MMap()

    /** Mutable set of neighbors */
    val neighbors: MSet[Cell] = MSet()

    /** Connects this cell to a neighbor. Used when initialize the model. */
    def connect_to(neighbor: Cell) {
      neighbors.add(neighbor)
      neighbor.neighbors.add(this)
    }

    /* Our underlying model is based on having various pheromone levels through
     * each cell. Pheromone levels will change through the following events
     * which happen each iteration.
     *
     * - Exponential decay. Pheromone levels are multiplied by a constant
     *   0 < beta < 1
     *
     * - Secretion. Objects within a cell, like food, obstacles, or plasmodium
     *   will deposit pheromone in the cell containing them.
     *
     * - Dissipation. This is based on Newton's Law of Cooling, where the
     *   flux (in this case of chemical concentration) is proportional to
     *   the gradient of concentration, with some constant of proportionality
     *   kappa.
     */

    /**
      * Implements an exponential decay on the cell's pheromone levels
      * by multiplying each pheromone level by a decay constant. Called by
      * [[physarum.Simulation]]'s update_pheromone method.
      */
    def decay_pheromone() {
      /* Iterate over (pheromone, level) pairs, mutating the levels
       * by multiplying them by the decay constant */
      pheromones.foreach({
        case (pheromone, level) => {
          pheromones.update(pheromone, level * Constants.decay_rate)
        }
      })
    }

    /** Adds some amount of pheromone to the current mapping. */
    def add_pheromone(pher: Pheromone, amount: Double) =
      pheromones.get(pher) match {
        case None => pheromones.update(pher, amount)
        case Some(existing_amount) => pheromones.update(pher, existing_amount + amount)
      }
    
    def secrete_pheromone() =
      objects.foreach(obj =>
        obj.scent.foreach({
          /* For each object in the cell, find its scent and add it to our
           * mapping. */
          case (pher, amount) => add_pheromone(pher, amount)
        })
      )

    def update_local_pheromone() {
      decay_pheromone()
      secrete_pheromone()
    }

    def save_pheromone() {
      current_pheromones.clear()
      current_pheromones ++= pheromones
    }

    def dissipate_pheromone() {
      val dissipation_constant = Constants.cell_permeability / 8

      def pheromone_gradient(a: Cell, b: Cell): Map[Pheromone, Double] = {
        val output: MMap[Pheromone, Double] = MMap()
        
        val (a_phers, b_phers) = (a.current_pheromones.keySet,
                                  b.current_pheromones.keySet)

        // Which pheromones these cells have in common
        val shared_pheromones = a_phers & b_phers

        // Which pheromones each of these cells has exclusively
        val (a_exclusive, b_exclusive) = (a_phers &~ b_phers, b_phers &~ a_phers)

        shared_pheromones.foreach(pher =>
          output.update(pher, b.current_pheromones(pher) - a.current_pheromones(pher)))

        /* If a pheromone is in cell A but not in cell B, that is a
         * negative gradient. If a that pheromone is in B but not A, that's
         * a positive gradient. */
        a_exclusive.foreach(pher =>
          output.update(pher, -a.current_pheromones(pher)))

        b_exclusive.foreach(pher =>
          output.update(pher, b.current_pheromones(pher)))

        output.toMap
      }

      /* Compute the pheromone gradient for each neighbor and update pheromone
       * levels in both cells.
       */
      neighbors.foreach(neighbor => {
        val gradient = pheromone_gradient(this, neighbor)
        gradient.foreach({
          case (pher, level) => {
            neighbor.add_pheromone(pher, -dissipation_constant * level)
            add_pheromone(pher, dissipation_constant * level)
          }
        })
      })
    }

    /** Compute the total amount of pheromone in this cell. At the moment,
      * this just reports the amount of Attract in the cell (or zero if there
      * is no Atrract). It could be any function of the various pheromone
      * amounts, however. */
    def total_pheromone: Double = pheromones.get(Attract) match {
      case None => 0.0
      case Some(x) => x
    }
    
    def contains_plasmodium: Boolean = objects.exists(obj =>
      obj.isInstanceOf[Plasmodium])

    def add_plasmodium() =
      if(!contains_plasmodium)
        objects.add(new Plasmodium(1.0))

    def remove_plasmodium() =
      if(contains_plasmodium)
        objects.retain(obj => !obj.isInstanceOf[Plasmodium])

    def total_food: Double =
      objects.filter(obj => obj.isInstanceOf[Food]).foldLeft(0.0)({
        case (acc, obj) => acc + obj.asInstanceOf[Food].amount
      })
  }

  /**
    * A simulation of Physarum polycephalum, or slime mold. Slime molds begin
    * their lives as spores and go into a vegetative state (the plasmodium)
    * which grows to cover food sources.

    * The basic principle of growth behind the plasmodium is that it tends to
    * get larger in the areas that are carrying the most resources. In this
    * way, the slime mold prioritizes tissues that are most immediately
    * useful to it.

    * In this simulation, we will model the utility of a given region in space
    * by leaving chemical markers (or pheromones) to indicate where the slime
    * mold will want to grow.
    */
  class Simulation extends PApplet {
    val (rows, cols) = (10, 10)

    /** The simulation's state is represented by a mutable 2D grid of cells */
    val grid: Array[Array[Cell]] = Array.ofDim(rows, cols)

    val random = new Random()

    /** Inherited from PApplet. Sets up the model and view states for the
      * simulation.
      */
    override def setup() {
      // Set up the model state
      grid.indices.foreach(row =>
        grid(row).indices.foreach(col =>
          grid(row)(col) = new Cell()
        )
      )

      // Connect the cells
      grid.indices.foreach(row =>
        grid(row).indices.foreach(col => {
          /* Calculate the indices of neighbors. Here we list the possible
           * values for the neighbor row and column indices. */
          val row_indices = List(row - 1, row, row + 1)
          val col_indices = List(col - 1, col, col + 1)

          // Now we take a Cartesian product using wonderful monads.
          val neighbor_indices = row_indices.flatMap(r =>
            col_indices.map(c => (r, c)))

          // Filter out the indices that our outside the bounds of our grid.
          neighbor_indices.filter({
            case (row, col) => (row >= 0) && (row < rows) &&
                               (col >= 0) && (col < cols)
          }).foreach({
            // Now connect the cell to its neighbors.
            case (row_index, col_index) =>
              grid(row)(col).connect_to(grid(row_index)(col_index))
          })
        }))
          

      size(1024, 768)
    }

    /** Inherited from PApplet. Draws the state of the simulation. */
    override def draw() {
      update_model()
      background(0x20, 0x20, 0x20)
    }

    def update_model() {
      update_pheromone()
      update_plasmodium()
    }

    /**
      * Iterates over all of the cells in the grid, updating their pheromone
      * levels */
    def update_pheromone() {
      // Dissipate the pheromone levels across cell boundaries
      grid.foreach(row =>
        row.foreach(cell =>
          cell.dissipate_pheromone()))

      // Iterate over each cell in the grid
      grid.foreach(row =>
        row.foreach(cell => {
          cell.update_local_pheromone()
          cell.save_pheromone()
        })
      )
    }

    def update_plasmodium() {
      def sigmoid(x: Double): Double = 1/(1 + math.exp(-x))

      val cells = grid.flatten

      def propagate_plasmodium() {
        // All cells containing plasmodium
        val plasmodium_cells = cells.filter(cell => cell.contains_plasmodium)

        /* All cells not containing plasmodium that are neighbored by cells
         * containing plasmodium. These are cells where the plasmodium may
         * expand this iteration.
         */
         val neighbor_cells = plasmodium_cells.flatMap(cell =>
           cell.neighbors.filter(neighbor =>
             !neighbor.contains_plasmodium)).toSet
         
         val neighbor_average_pheromone = neighbor_cells.foldLeft(0.0)({
           case (acc, cell) => acc + cell.total_pheromone
         })/neighbor_cells.size
         
         neighbor_cells.foreach(cell => {
           val difference = cell.total_pheromone - neighbor_average_pheromone
           val probability = sigmoid(2 * difference)
           if(random.nextDouble() < probability)
             cell.add_plasmodium()
         })
      }

      def chisel_plasmodium() {
        def select_from_pmf[A](random: Random, pmf: Map[A, Double]): A = {
          val r = random.nextDouble()
          var sum = 0.0
          pmf.foreach({
            case (event, prob) => {
              sum = sum + prob
              if(sum > r)
                return event
            }
          })

          // If we get here, the sum of all probabilities is less than 1.
          println(pmf)
          throw new Exception("Given map is not a PMF")
        }

        // All cells containing plasmodium
        val plasmodium_cells = cells.filter(cell => cell.contains_plasmodium)

        val nonessential_cells = plasmodium_cells.filter(cell =>
          cell.total_food < 1)

        /* Find the number of cells we can sustain by calculating the
         * total colonized food */
        val sustainable_cells = plasmodium_cells.foldLeft(0.0)({
          case (acc, cell) => acc + cell.total_food
        }).toInt

        if(plasmodium_cells.size > sustainable_cells) {
          val total_pheromone = plasmodium_cells.foldLeft(0.0)({
            case (acc, cell) => acc + cell.total_pheromone
          })

          if(total_pheromone > 0) {
            // Calculate the probability that each nonessential cell will
            // be removed
            val probabilities = nonessential_cells.map(cell =>
              1 - cell.total_pheromone / total_pheromone)

            val pmf = nonessential_cells.zip(probabilities).toMap
            select_from_pmf(random, pmf).remove_plasmodium()
          }
        }

        if(plasmodium_cells.size > (sustainable_cells + 1)) {
          chisel_plasmodium()
        }
      }

      propagate_plasmodium()
      chisel_plasmodium()
    }
  }

  /** A simple object which launches [[physarum.Simulation]]. */
  object Main {
    def main(args: Array[String]) {
      PApplet.main("physarum.Simulation")
    }
  }
}
