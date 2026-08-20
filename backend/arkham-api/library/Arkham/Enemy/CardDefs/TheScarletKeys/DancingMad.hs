module Arkham.Enemy.CardDefs.TheScarletKeys.DancingMad where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

desiderioDelgadoAlvarez106 :: CardDef
desiderioDelgadoAlvarez106 =
  (enemy "09606" ("Desiderio Delgado Álvarez" <:> "The Man in the Blood-Soaked Suit") DancingMad 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 3
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Coterie, Elite]
    , cdKeywords =
        setFromList
          [ Keyword.Alert
          , Keyword.Concealed DesiderioDelgadoAlvarez (PerPlayer 1)
          , Keyword.Hunter
          , Keyword.Retaliate
          ]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    }

desiderioDelgadoAlvarez107 :: CardDef
desiderioDelgadoAlvarez107 =
  (enemy "09607" ("Desiderio Delgado Álvarez" <:> "The Man in the Blood-Soaked Suit") DancingMad 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 3
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Coterie, Elite]
    , cdKeywords =
        setFromList
          [ Keyword.Alert
          , Keyword.Concealed DesiderioDelgadoAlvarez (PerPlayer 1)
          , Keyword.Hunter
          , Keyword.Retaliate
          ]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    , cdDoubleSided = True
    , cdOtherSide = Just "09607b"
    }
