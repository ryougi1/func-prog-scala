package barneshut

import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.mutable.ParHashSet
import scala.collection.parallel.{Combiner, TaskSupport}
import scala.{collection => coll}

class Simulator(val taskSupport: TaskSupport, val timeStats: TimeStatistics) {

  def updateBoundaries(boundaries: Boundaries, body: Body): Boundaries = {
    val updated = new Boundaries
    updated.maxX = math.max(boundaries.maxX, body.x)
    updated.minX = math.min(boundaries.minX, body.x)
    updated.maxY = math.max(boundaries.maxY, body.y)
    updated.minY = math.min(boundaries.minY, body.y)
    updated
  }

  def mergeBoundaries(a: Boundaries, b: Boundaries): Boundaries = {
    val updated = new Boundaries
    updated.maxX = math.max(a.maxX, b.maxX)
    updated.minX = math.min(a.minX, b.minX)
    updated.maxY = math.max(a.maxY, b.maxY)
    updated.minY = math.min(a.minY, b.minY)
    updated
  }

  def computeBoundaries(bodies: coll.Seq[Body]): Boundaries = timeStats.timed("boundaries") {
    val parBodies = bodies.par
    parBodies.tasksupport = taskSupport
    parBodies.aggregate(new Boundaries)(updateBoundaries, mergeBoundaries)
  }

  /**
    * Aggregate the SectorMatrix from the sequence of bodies, the same way it
    * was used for boundaries. Use the SECTOR_PRECISION constant when creating
    * a new SectorMatrix.
    *
    * Aggregate signature:
    * def aggregate[B](z: ⇒ B)(seqop: (B, A) ⇒ B, combop: (B, B) ⇒ B): B
    */
  def computeSectorMatrix(bodies: coll.Seq[Body], boundaries: Boundaries): SectorMatrix = timeStats.timed("matrix") {
    val parBodies = bodies.par
    parBodies.tasksupport = taskSupport
    val sectorMatrix = new SectorMatrix(boundaries, SECTOR_PRECISION)
    parBodies.aggregate(sectorMatrix)(
      (sectorMatrix: SectorMatrix, body: Body) => sectorMatrix += body,
      (a: SectorMatrix, b: SectorMatrix) => a.combine(b))
  }

  def computeQuad(sectorMatrix: SectorMatrix): Quad = timeStats.timed("quad") {
    sectorMatrix.toQuad(taskSupport.parallelismLevel)
  }

  def updateBodies(bodies: coll.Seq[Body], quad: Quad): coll.Seq[Body] = timeStats.timed("update") {
    val parBodies = bodies.par
    parBodies.tasksupport = taskSupport
    parBodies.map(x => x.updated(quad)).seq
  }

  def eliminateOutliers(bodies: coll.Seq[Body], sectorMatrix: SectorMatrix, quad: Quad): coll.Seq[Body] = timeStats.timed("eliminate") {
    def isOutlier(b: Body): Boolean = {
      val dx = quad.massX - b.x
      val dy = quad.massY - b.y
      val d = math.sqrt(dx * dx + dy * dy)
      // object is far away from the center of the mass
      if (d > eliminationThreshold * sectorMatrix.boundaries.size) {
        val nx = dx / d
        val ny = dy / d
        val relativeSpeed = b.xspeed * nx + b.yspeed * ny
        // object is moving away from the center of the mass
        if (relativeSpeed < 0) {
          val escapeSpeed = math.sqrt(2 * gee * quad.mass / d)
          // object has the espace velocity
          -relativeSpeed > 2 * escapeSpeed
        } else false
      } else false
    }

    def outliersInSector(x: Int, y: Int): Combiner[Body, ParHashSet[Body]] = {
      val combiner = ParHashSet.newCombiner[Body]
      combiner ++= sectorMatrix(x, y).filter(isOutlier)
      combiner
    }

    val sectorPrecision = sectorMatrix.sectorPrecision
    val horizontalBorder = for (x <- 0 until sectorPrecision; y <- Seq(0, sectorPrecision - 1)) yield (x, y)
    val verticalBorder = for (y <- 1 until sectorPrecision - 1; x <- Seq(0, sectorPrecision - 1)) yield (x, y)
    val borderSectors = horizontalBorder ++ verticalBorder

    // compute the set of outliers
    val parBorderSectors = borderSectors.par
    parBorderSectors.tasksupport = taskSupport
    val outliers = parBorderSectors.map({ case (x, y) => outliersInSector(x, y) }).reduce(_ combine _).result

    // filter the bodies that are outliers
    val parBodies = bodies.par
    parBodies.filter(!outliers(_)).seq
  }

  def step(bodies: coll.Seq[Body]): (coll.Seq[Body], Quad) = {
    // 1. compute boundaries
    val boundaries = computeBoundaries(bodies)
    println("finished computing boundaries")

    // 2. compute sector matrix
    val sectorMatrix = computeSectorMatrix(bodies, boundaries)
    println("finished computing sector matrix")

    // 3. compute quad tree
    val quad = computeQuad(sectorMatrix)
    println("finished computing quad tree")

    // 4. eliminate outliers
    val filteredBodies = eliminateOutliers(bodies, sectorMatrix, quad)
    println("finished eliminating outliers")

    // 5. update body velocities and positions
    val newBodies = updateBodies(filteredBodies, quad)
    println("finished updating body velocities and positions")

    (newBodies, quad)
  }

}
