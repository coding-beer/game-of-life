package codingBeer.gameOfLife

trait Cell 
object Alive extends Cell { override def toString = "*" }
object Dead extends Cell { override def toString = "." }

object Environments {

  type Point = (Int, Int)
  type Environment = Map[Point, Cell]

  def nextDay(environment: Environment): Environment = environment.map { keyValue =>
    val (point, _) = keyValue
    point -> nextState(point, environment)
  }.toMap 

  def nextState(point: Point, environment: Environment): Cell = neighboursAlive(point, environment) match {
    case 3 => Alive
    case 2 if(environment(point) == Alive) => Alive
    case _ => Dead
  }

  def neighboursAlive(point: Point, environment: Environment): Int =
    neighboursOf(point, environment).count((cell: Cell) => cell == Alive)

  def neighboursOf(point: Point, environment: Environment): Seq[Cell] = {
    val (y, x) = point
    for (row <- y - 1 to y + 1;
      column <- x - 1 to x + 1;
      if (row, column) != point && environment.contains (row, column)
    ) yield environment (row, column)
  }

}

object Main extends App {

  import Thread._
  import Environments._
  import sys.process._
  import scala.annotation.tailrec

  val env = Map(
    (0,0) -> Dead, (0,1) -> Dead,  (0,2) -> Dead,  (0,3) -> Dead,  (0,4) -> Dead, 
    (1,0) -> Dead, (1,1) -> Dead,  (1,2) -> Dead,  (1,3) -> Dead,  (1,4) -> Dead, 
    (2,0) -> Dead, (2,1) -> Alive, (2,2) -> Alive, (2,3) -> Alive, (2,4) -> Dead, 
    (3,0) -> Dead, (3,1) -> Dead,  (3,2) -> Dead,  (3,3) -> Dead,  (3,4) -> Dead, 
    (4,0) -> Dead, (4,1) -> Dead,  (4,2) -> Dead,  (4,3) -> Dead,  (4,4) -> Dead
  )
  
  @tailrec def iter(env: Environment) {
    "clear".!

    for (row <- 0 until 5) {
      for (column <- 0 until 5) print(env(row, column))
      println();
    }

    sleep(500)
    iter(nextDay(env))
  }

  iter(env)
}
