{- | The Masque of the Red Death's act deck.

Two acts, each ending on a room: act 1 wants the whole undefeated party in the
Black Chamber at the end of a round, act 2 wants them all back out through the
front door. Neither prints a clue requirement -- the clues are spent at the
chamber doors instead.
-}
module Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Acts where

import Arkham.Act.CardDefs.Import
import Arkham.Homebrew.TheMasqueOfTheRedDeath.Sets qualified as Set

theSevenChambers :: CardDef
theSevenChambers =
  act ":the-masque-of-the-red-death:005" "The Seven Chambers" 1 Set.TheMasqueOfTheRedDeath

theMidnightHour :: CardDef
theMidnightHour =
  act ":the-masque-of-the-red-death:006" "The Midnight Hour" 2 Set.TheMasqueOfTheRedDeath
