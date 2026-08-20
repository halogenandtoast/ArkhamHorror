module Arkham.Enemy.CardDefs.ReturnToThePathToCarcosa.ReturnToTheUnspeakableOath where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

hostOfInsanity :: CardDef
hostOfInsanity =
  (enemy "52037" "Host of Insanity" ReturnToTheUnspeakableOath 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 4
    , cdHealth = health 4
    , cdCardTraits = setFromList [Avatar, Elite]
    , cdKeywords = setFromList [Keyword.Massive, Keyword.Hunter]
    , cdVictoryPoints = Just 1
    }
