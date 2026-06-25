module Arkham.Enemy.CardDefs.BadBlood where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword
import Arkham.Token (Token (Memory))

elspethBaudin :: CardDef
elspethBaudin =
  unique
    $ doubleSided "90023b"
    $ (enemy "90023" ("Elspeth Baudin" <:> "Hyperborean Sorceress") BadBlood 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 8
      , cdEvade = evade 8
      , cdHealth = healthPerInvestigator 4
      , cdCardTraits = setFromList [Humanoid, Sorcerer, Elite]
      , cdKeywords =
          setFromList
            [ Keyword.Alert
            , Keyword.Patrol (LocationWithToken Memory)
            , Keyword.Retaliate
            ]
      }
