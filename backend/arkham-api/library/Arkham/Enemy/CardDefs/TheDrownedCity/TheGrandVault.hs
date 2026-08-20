module Arkham.Enemy.CardDefs.TheDrownedCity.TheGrandVault where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

slithererInDarkness :: CardDef
slithererInDarkness =
  unique
    $ (enemy "11605" ("Slitherer in Darkness" <:> "Lurker From Below") TheGrandVault 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 4
      , cdHealth = health 5
      , cdCardTraits = setFromList [Monster, Stowaway, Elite]
      , cdVictoryPoints = Just 1
      }

vaultAttendant :: CardDef
vaultAttendant =
  (enemy "11606" "Vault Attendant" TheGrandVault 3)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 2
    , cdEvade = evade 4
    , cdHealth = health 2
    , cdCardTraits = setFromList [Monster, Keeper]
    , cdKeywords = setFromList [Keyword.Hunter]
    }
