import processing.core.PApplet
import scala.collection.mutable.{Set => MSet, Map => MMap}

package physarum {
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
  class Food(amount: Double) extends Odiferous {
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
    /** The simulation's state is represented by a mutable 2D grid of cells */
    val grid: Array[Array[Cell]] = Array.ofDim(10, 10)

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

      size(1024, 768)
    }

    /** Inherited from PApplet. Draws the state of the simulation. */
    override def draw() {
      update_model()
      background(0x20, 0x20, 0x20)
    }

    def update_model() {
      update_pheromone()
    }

    /**
      * Iterates over all of the cells in the grid, updating their pheromone
      * levels */
    def update_pheromone() {
      // Iterate over each cell in the grid
      grid.foreach(row =>
        row.foreach(cell =>
          /* For each cell, update its pheromone levels with each object
           * it contains */
          cell.objects.foreach(obj => {
          })
        )
      )
    }
  }

  /** A simple class which launches [[physarum.Simulation]]. */
  object Main {
    def main(args: Array[String]) {
      PApplet.main("physarum.Simulation")
    }
  }
}
