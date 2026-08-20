module Arkham.Enemy.CardDefs.ReturnToThePathToCarcosa.ReturnToCurtainCall where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

laComtesseSubverterOfPlans :: CardDef
laComtesseSubverterOfPlans =
  unique
    $ (enemy "52020" ("La Comtesse" <:> "Subverter of Plans") ReturnToCurtainCall 1)
      { cdSanityDamage = sanityDamage 1
      , cdFight = fight 1
      , cdEvade = evade 3
      , cdHealth = health 3
      , cdCardTraits = setFromList [Humanoid, Servitor]
      , cdKeywords = setFromList [Keyword.Hunter]
      }
