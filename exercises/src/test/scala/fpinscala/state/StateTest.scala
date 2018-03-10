package fpinscala.state

import org.scalatest._
import prop._

class StateTest extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {

  property("nonNegativeInt should generate non negative numbers") {
    forAll { seed: Long =>
      val rng = RNG.Simple(seed)
      val (i, _) = RNG.nonNegativeInt(rng)
      i should be >= 0
    }
  }

  property("double should generate double between 0 and 1") {
    forAll { seed: Long =>
      val rng = RNG.Simple(seed)
      val (d, _) = RNG.double(rng)
      d should be >= 0.0
      d should be < 1.0
    }
  }

  property("double and double2 should generate the same numbers") {
    forAll { seed: Long =>
      val rng = RNG.Simple(seed)
      val (d1, _) = RNG.double(rng)
      val (d2, _) = RNG.double2(rng)
      d1 should equal (d2)
    }
  }


}