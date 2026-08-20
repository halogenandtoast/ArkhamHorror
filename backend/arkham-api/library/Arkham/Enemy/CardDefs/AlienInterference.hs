module Arkham.Enemy.CardDefs.AlienInterference where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

otherworldlyMeddler :: CardDef
otherworldlyMeddler =
  (enemy "84029" ("Otherworldly Meddler" <:> "Presence from Beyond the Stars") AlienInterference 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 4
    , cdEvade = evade 3
    , cdHealth = healthPerInvestigator 5
    , cdCardTraits = setFromList [Monster, MiGo, Elite]
    , cdKeywords = setFromList [Keyword.Retaliate]
    , cdVictoryPoints = Just 2
    , cdUnique = True
    }
