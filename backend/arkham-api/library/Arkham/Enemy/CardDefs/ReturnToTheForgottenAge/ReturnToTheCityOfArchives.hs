module Arkham.Enemy.CardDefs.ReturnToTheForgottenAge.ReturnToTheCityOfArchives where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

captiveSubjects :: CardDef
captiveSubjects =
  (enemy "53058" "Captive Subjects" ReturnToTheCityOfArchives 2)
    { cdSanityDamage = sanityDamage 2
    , cdFight = fight 2
    , cdEvade = evade 4
    , cdHealth = health 5
    , cdCardTraits = singleton Monster
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Retaliate]
    }
