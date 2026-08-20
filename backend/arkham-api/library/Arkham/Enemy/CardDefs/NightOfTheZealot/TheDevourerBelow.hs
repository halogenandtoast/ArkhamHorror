module Arkham.Enemy.CardDefs.NightOfTheZealot.TheDevourerBelow where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

umordhoth :: CardDef
umordhoth =
  unique
    $ (enemy "01157" ("Umôrdhoth" <:> "The Devourer Below") TheDevourerBelow 1)
      { cdHealthDamage = healthDamage 3
      , cdSanityDamage = sanityDamage 3
      , cdFight = fight 5
      , cdEvade = evade 6
      , cdHealth = health 6
      , cdCardTraits = setFromList [AncientOne, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
      }
