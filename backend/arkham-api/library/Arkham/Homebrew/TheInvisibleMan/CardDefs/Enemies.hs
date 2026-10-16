module Arkham.Homebrew.TheInvisibleMan.CardDefs.Enemies where

import Arkham.Enemy.CardDefs.Import
import Arkham.Homebrew.TheInvisibleMan.Sets qualified as Set
import Arkham.Keyword qualified as Keyword

griffin :: CardDef
griffin =
  (enemy ":the-invisible-man:016" ("Griffin" <:> "The Invisible Man") Set.TheInvisibleMan 1)
    { cdCardTraits = setFromList [Humanoid, Criminal, Elite]
    , cdFight = fightUnknown
    , cdHealth = Nothing
    , cdEvade = evadeUnknown
    , cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdUnique = True
    , cdVictoryPoints = Just 2
    }

maddenedBeast :: CardDef
maddenedBeast =
  (enemy ":the-invisible-man:029" "Maddened Beast" Set.TheInvisibleMan 2)
    { cdCardTraits = singleton Creature
    , cdFight = fight 4
    , cdHealth = health 2
    , cdEvade = evade 2
    , cdHealthDamage = healthDamage 1
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }

sentientSolution :: CardDef
sentientSolution =
  (enemy ":the-invisible-man:034" "Sentient Solution" Set.TheInvisibleMan 2)
    { cdCardTraits = setFromList [Monster, Abomination]
    , cdFight = fight 2
    , cdHealth = health 3
    , cdEvade = evade 2
    , cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdKeywords = singleton Keyword.Hunter
    }

sheldonSupplier :: CardDef
sheldonSupplier =
  (enemy ":the-invisible-man:035" "Sheldon Supplier" Set.TheInvisibleMan 2)
    { cdCardTraits = setFromList [Humanoid, Criminal]
    , cdFight = fight 3
    , cdHealth = health 2
    , cdEvade = evade 3
    , cdHealthDamage = healthDamage 1
    }

translucentExperiment :: CardDef
translucentExperiment =
  (enemy ":the-invisible-man:037" "Translucent Experiment" Set.TheInvisibleMan 2)
    { cdCardTraits = singleton Creature
    , cdFight = fight 2
    , cdHealth = health 2
    , cdEvade = evade 4
    , cdSanityDamage = sanityDamage 1
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Alert]
    }
