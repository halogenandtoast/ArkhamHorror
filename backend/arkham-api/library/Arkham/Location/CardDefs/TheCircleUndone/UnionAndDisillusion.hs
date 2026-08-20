module Arkham.Location.CardDefs.TheCircleUndone.UnionAndDisillusion where

import Arkham.Location.CardDefs.Import

forbiddingShore :: CardDef
forbiddingShore =
  location
    "05250"
    "Forbidding Shore"
    [Woods]
    Moon
    [Triangle, Squiggle]
    UnionAndDisillusion

miskatonicRiver :: CardDef
miskatonicRiver =
  location
    "05249"
    "Miskatonic River"
    [River]
    Triangle
    [Moon]
    UnionAndDisillusion

theGeistTrap :: CardDef
theGeistTrap =
  victory 1
    $ location
      "05257"
      "The Geist-Trap"
      [Woods, Spectral]
      Plus
      [Squiggle]
      UnionAndDisillusion

unvisitedIsleDecayedWillow :: CardDef
unvisitedIsleDecayedWillow =
  victory 1
    $ locationWithUnrevealed
      "05256"
      "Unvisited Isle"
      [Woods]
      Squiggle
      [Squiggle, Moon, Plus]
      ("Unvisited Isle" <:> "Decayed Willow")
      [Woods]
      Squiggle
      [Squiggle, Moon, Plus]
      UnionAndDisillusion

unvisitedIsleForsakenWoods :: CardDef
unvisitedIsleForsakenWoods =
  victory 1
    $ locationWithUnrevealed
      "05253"
      "Unvisited Isle"
      [Woods]
      Squiggle
      [Squiggle, Moon, Plus]
      ("Unvisited Isle" <:> "Forsaken Woods")
      [Woods]
      Squiggle
      [Squiggle, Moon, Plus]
      UnionAndDisillusion

unvisitedIsleHauntedSpring :: CardDef
unvisitedIsleHauntedSpring =
  victory 1
    $ locationWithUnrevealed
      "05255"
      "Unvisited Isle"
      [Woods]
      Squiggle
      [Squiggle, Moon, Plus]
      ("Unvisited Isle" <:> "Haunted Spring")
      [Woods]
      Squiggle
      [Squiggle, Moon, Plus]
      UnionAndDisillusion

unvisitedIsleMistyClearing :: CardDef
unvisitedIsleMistyClearing =
  victory 1
    $ locationWithUnrevealed
      "05252"
      "Unvisited Isle"
      [Woods]
      Squiggle
      [Squiggle, Moon, Plus]
      ("Unvisited Isle" <:> "Misty Clearing")
      [Woods]
      Squiggle
      [Squiggle, Moon, Plus]
      UnionAndDisillusion

unvisitedIsleMossCoveredSteps :: CardDef
unvisitedIsleMossCoveredSteps =
  victory 1
    $ locationWithUnrevealed
      "05254"
      "Unvisited Isle"
      [Woods]
      Squiggle
      [Squiggle, Moon, Plus]
      ("Unvisited Isle" <:> "Moss-Covered Steps")
      [Woods]
      Squiggle
      [Squiggle, Moon, Plus]
      UnionAndDisillusion

unvisitedIsleStandingStones :: CardDef
unvisitedIsleStandingStones =
  victory 1
    $ locationWithUnrevealed
      "05251"
      "Unvisited Isle"
      [Woods]
      Squiggle
      [Squiggle, Moon, Plus]
      ("Unvisited Isle" <:> "Standing Stones")
      [Woods]
      Squiggle
      [Squiggle, Moon, Plus]
      UnionAndDisillusion
