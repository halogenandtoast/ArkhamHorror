module Arkham.Enemy.CardDefs.TheForgottenAge.TheBoundaryBeyond where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

handOfTheBrotherhood :: CardDef
handOfTheBrotherhood =
  (enemy "04188" "Hand of the Brotherhood" TheBoundaryBeyond 2)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Cultist]
    }

padmaAmrita :: CardDef
padmaAmrita =
  unique
    $ (enemy "04186" ("Padma Amrita" <:> "Cold-Blooded Charmer") TheBoundaryBeyond 1)
      { cdFight = fight 5
      , cdEvade = evade 3
      , cdHealth = healthPerInvestigator 3
      , cdCardTraits = setFromList [Humanoid, Serpent, Servitor, Elite]
      , cdVictoryPoints = Just 2
      , cdVengeancePoints = Just 2
      , cdKeywords = setFromList [Keyword.Alert, Keyword.Retaliate, Keyword.Hunter]
      }

serpentOfTenochtitlan :: CardDef
serpentOfTenochtitlan =
  (enemy "04187" "Serpent of Tenochtitlán" TheBoundaryBeyond 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 5
    , cdCardTraits = setFromList [Humanoid, Monster, Serpent]
    , cdVictoryPoints = Just 1
    , cdVengeancePoints = Just 1
    }
