module Arkham.Homebrew.TheLegendOfSleepyHollow.CardDefs.Enemies where

import Arkham.Enemy.CardDefs.Import
import Arkham.Homebrew.TheLegendOfSleepyHollow.Sets qualified as Set
import Arkham.Keyword qualified as Keyword

theHeadlessHorseman :: CardDef
theHeadlessHorseman =
  ( enemy
      ":the-legend-of-sleepy-hollow:012"
      ("The Headless Horseman" <:> "The Spectre of Sleepy Hollow")
      Set.TheLegendOfSleepyHollow
      1
  )
    { cdCardTraits = setFromList [Monster, Geist, Elite]
    , cdFight = fight 3
    , cdHealth = healthPerInvestigator 2
    , cdEvade = evade 3
    , cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdKeywords = singleton Keyword.Hunter
    , cdUnique = True
    , cdVictoryPoints = Just 2
    }

decapitatedVictim :: CardDef
decapitatedVictim =
  (enemy ":the-legend-of-sleepy-hollow:018" "Decapitated Victim" Set.TheLegendOfSleepyHollow 3)
    { cdCardTraits = setFromList [Monster, Geist]
    , cdFight = fight 2
    , cdHealth = health 1
    , cdEvade = evade 3
    , cdSanityDamage = sanityDamage 2
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Aloof]
    }

disembodiedSkull :: CardDef
disembodiedSkull =
  (enemy ":the-legend-of-sleepy-hollow:019" "Disembodied Skull" Set.TheLegendOfSleepyHollow 2)
    { cdCardTraits = setFromList [Monster, Geist]
    , cdFight = fight 1
    , cdHealth = health 1
    , cdEvade = evade 3
    , cdSanityDamage = sanityDamage 1
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Aloof]
    }

hessianSpectre :: CardDef
hessianSpectre =
  (enemy ":the-legend-of-sleepy-hollow:020" "Hessian Spectre" Set.TheLegendOfSleepyHollow 3)
    { cdCardTraits = setFromList [Monster, Geist]
    , cdFight = fight 4
    , cdHealth = health 3
    , cdEvade = evade 2
    , cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Alert]
    }

secretiveHag :: CardDef
secretiveHag =
  (enemy ":the-legend-of-sleepy-hollow:025" "Secretive Hag" Set.TheLegendOfSleepyHollow 2)
    { cdCardTraits = setFromList [Humanoid, Witch]
    , cdFight = fight 3
    , cdHealth = health 2
    , cdEvade = evade 4
    , cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Aloof]
    }

witchOfTheHollow :: CardDef
witchOfTheHollow =
  (enemy ":the-legend-of-sleepy-hollow:027" "Witch of the Hollow" Set.TheLegendOfSleepyHollow 3)
    { cdCardTraits = setFromList [Humanoid, Witch]
    , cdFight = fight 3
    , cdHealth = health 3
    , cdEvade = evade 2
    , cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdKeywords = singleton Keyword.Retaliate
    }
