module Arkham.Enemy.CardDefs.TheScarletKeys.CongressOfTheKeys where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

mimeticNemesisInfiltratorOfRealities :: CardDef
mimeticNemesisInfiltratorOfRealities =
  (enemy "09715" ("Mimetic Nemesis" <:> "Infiltrator of Realities") CongressOfTheKeys 1)
    { cdHealthDamage = healthDamage 3
    , cdSanityDamage = sanityDamage 3
    , cdFight = fight 5
    , cdEvade = evade 5
    , cdHealth = health 3
    , cdCardTraits = setFromList [AncientOne, Outsider, Elite]
    , cdKeywords = singleton Keyword.Massive
    , cdUnique = True
    }
