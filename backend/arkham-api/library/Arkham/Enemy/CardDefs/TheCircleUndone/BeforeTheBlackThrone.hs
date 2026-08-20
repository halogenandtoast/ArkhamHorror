module Arkham.Enemy.CardDefs.TheCircleUndone.BeforeTheBlackThrone where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

azathoth :: CardDef
azathoth =
  (enemy "05346" ("Azathoth" <:> "The Primal Chaos") BeforeTheBlackThrone 1)
    { cdHealthDamage = healthDamage 3
    , cdSanityDamage = sanityDamage 3
    , cdCardTraits = setFromList [AncientOne, Elite]
    , cdUnique = True
    }

mindlessDancer :: CardDef
mindlessDancer =
  (enemy "05341" "Mindless Dancer" BeforeTheBlackThrone 3)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 6
    , cdEvade = evade 3
    , cdHealth = health 5
    , cdCardTraits = singleton Monster
    , cdKeywords = singleton Keyword.Hunter
    }
