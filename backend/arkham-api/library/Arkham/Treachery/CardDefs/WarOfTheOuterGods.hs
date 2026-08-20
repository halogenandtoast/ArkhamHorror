module Arkham.Treachery.CardDefs.WarOfTheOuterGods where

import Arkham.Treachery.CardDefs.Import

deathAndDecay :: CardDef
deathAndDecay =
  (treachery "86026" "Death and Decay" WarOfTheOuterGods 2)
    { cdCardTraits = singleton Hex
    }

feastOfLocusts :: CardDef
feastOfLocusts =
  (treachery "86030" "Feast of Locusts" WarOfTheOuterGods 2)
    { cdCardTraits = singleton Hazard
    }

hellfire :: CardDef
hellfire =
  (treachery "86031" "Hellfire" WarOfTheOuterGods 3)
    { cdCardTraits = singleton Hazard
    }

huntDown :: CardDef
huntDown =
  (treachery "86045" "Hunt Down" ChildrenOfParadise 3)
    { cdCardTraits = singleton Hazard
    }

inevitableEnd :: CardDef
inevitableEnd =
  (treachery "86039" "Inevitable End" DeathOfStars 3)
    { cdCardTraits = singleton Hex
    }

predatorsCall :: CardDef
predatorsCall =
  (treachery "86028" "Predator's Call" WarOfTheOuterGods 2)
    { cdCardTraits = singleton Scheme
    }

ravagesOfWar :: CardDef
ravagesOfWar =
  (treachery "86032" "Ravages of War" WarOfTheOuterGods 2)
    { cdCardTraits = singleton Terror
    }

transmogrify :: CardDef
transmogrify =
  (treachery "86050" "Transmogrify" SwarmOfAssimilation 2)
    { cdCardTraits = singleton Curse
    }

whileTheySleep :: CardDef
whileTheySleep =
  (treachery "86033" "While They Sleep" WarOfTheOuterGods 2)
    { cdCardTraits = singleton Omen
    }
