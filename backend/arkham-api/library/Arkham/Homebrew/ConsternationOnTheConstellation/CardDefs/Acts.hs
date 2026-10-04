{- | Consternation on the Constellation's act deck.

No act has a clue requirement; each prints an @Objective@ instead. Act 3 is the
mirror of the agenda 3 branch: "Flee the Ship" when act 2 advanced first, "Plug
the Abyss" when agenda 2 did.
-}
module Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Acts where

import Arkham.Act.CardDefs.Import
import Arkham.Homebrew.ConsternationOnTheConstellation.Sets qualified as Set

thinkFast :: CardDef
thinkFast = act ":consternation-on-the-constellation:006" "Think Fast!" 1 Set.ConsternationOnTheConstellation

findItFirst :: CardDef
findItFirst =
  act ":consternation-on-the-constellation:007" "Find It First" 2 Set.ConsternationOnTheConstellation

-- | Act 3a when act 2 advanced first. Escape by lifeboat.
fleeTheShip :: CardDef
fleeTheShip =
  act ":consternation-on-the-constellation:008" "Flee the Ship" 3 Set.ConsternationOnTheConstellation

-- | Act 3a when agenda 2 advanced first. Destroy the tablet.
plugTheAbyss :: CardDef
plugTheAbyss =
  act ":consternation-on-the-constellation:009" "Plug the Abyss" 3 Set.ConsternationOnTheConstellation
