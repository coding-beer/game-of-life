package codingBeer.gameOfLife

import org.specs2.mutable._
import org.specs2.specification.Scope
import Environments._

class GameOfLifeSpec extends Specification {

  trait populatedEnvironment extends Scope {
    val environment = Map(
      (0,0) -> Alive, (0,1) -> Dead,  (0,2) -> Dead,
      (1,0) -> Dead,  (1,1) -> Alive, (1,2) -> Alive,
      (2,0) -> Alive, (2,1) -> Dead,  (2,2) -> Alive
    ) 
  }

  "Requesting neighbours for an environment" should {
    "return all 8 neighbours if it is not on the edges" in new populatedEnvironment {
      neighboursOf((1,1), environment).size must be equalTo 8
    }

    "return 5 neighbours if it is on the top" in new populatedEnvironment {
      neighboursOf((0,1), environment).size must be equalTo 5
    }
    
    "return 3 neighbours if it is on the corner" in new populatedEnvironment {
      neighboursOf((0,0), environment).size must be equalTo 3
    }

    "returns how many neighbours alive" in new populatedEnvironment {
      neighboursAlive((0,1), environment) must be equalTo 3
    }
  }
  
  "A cell" should {
    "die by under population" in new populatedEnvironment {
      nextState((0,0), environment) must be equalTo Dead
    }

    "die by over population" in new populatedEnvironment {
      nextState((1,1), environment) must be equalTo Dead
    }

    "reborn if has 3 alive neighbours" in new populatedEnvironment {
      nextState((1,0), environment) must be equalTo Alive
    }

    "stay alive with 2 alives neighbours" in new populatedEnvironment {
      nextState((1,2), environment) must be equalTo Alive
    }

    "stay dead with 2 alives neighbours" in new populatedEnvironment {
      nextState((0,2), environment) must be equalTo Dead
    }
  }

  "The whole environment" should {

    "be processed" in new populatedEnvironment {
      val newDay = Map(
        (0,0) -> Dead, (0,1) -> Alive, (0,2) -> Dead,
        (1,0) -> Alive, (1,1) -> Dead, (1,2) -> Alive,
        (2,0) -> Dead, (2,1) -> Dead, (2,2) -> Alive
      )

      nextDay(environment) must be equalTo newDay
    }

  }
}
