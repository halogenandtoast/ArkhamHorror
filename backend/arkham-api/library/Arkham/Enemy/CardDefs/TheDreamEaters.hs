{- HLINT ignore "Use camelCase" -}
module Arkham.Enemy.CardDefs.TheDreamEaters where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

tonysQuarry :: CardDef
tonysQuarry =
  (weakness "06012" "Tony's Quarry")
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 4
    , cdEvade = evade 1
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Monster, DeepOne]
    , cdKeywords = setFromList [Keyword.Aloof]
    }

watcherFromAnotherDimension :: CardDef
watcherFromAnotherDimension =
  unique
    $ (weakness "06017" "Watcher from Another Dimension")
      { cdHealthDamage = healthDamage 3
      , cdFight = fight 5
      , cdEvade = evade 5
      , cdHealth = health 2
      , cdCardTraits = setFromList [Monster, Extradimensional]
      , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden, Keyword.Hunter]
      , cdRevelation = IsRevelation
      }

guardianOfTheCrystallizer :: CardDef
guardianOfTheCrystallizer =
  (weakness "06025" "Guardian of the Crystallizer")
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = singleton Monster
    , cdKeywords = setFromList [Keyword.Bonded 1 "06024", Keyword.Hunter]
    }

yourWorstNightmare :: CardDef
yourWorstNightmare =
  (basicWeakness "06038" "Your Worst Nightmare")
    { cdSanityDamage = sanityDamage 2
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = singleton Monster
    , cdKeywords = singleton Keyword.Hunter
    , cdDeckRestrictions = [MultiplayerOnly]
    }

unboundBeast :: CardDef
unboundBeast =
  (weakness "06283" "Unbound Beast")
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Extradimensional, Tindalos]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    , cdRevelation = IsRevelation
    }
