module Arkham.Enemy.CardDefs.Base where

import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.ClassSymbol
import Arkham.EncounterSet hiding (Blight, Byakhee, Dreamlands)
import Arkham.GameValue
import Arkham.Name
import Arkham.Prelude

baseEnemy
  :: CardCode
  -> Name
  -> Maybe (EncounterSet, Int)
  -> Maybe CardSubType
  -> CardDef
baseEnemy cardCode name mEncounterSet isWeakness =
  (emptyCardDef cardCode name $ if isJust isWeakness then PlayerEnemyType else EnemyType)
    { cdCardSubType = isWeakness
    , cdClassSymbols = if isJust isWeakness then singleton Neutral else mempty
    , cdEncounterSet = fst <$> mEncounterSet
    , cdEncounterSetQuantity = snd <$> mEncounterSet
    , cdLevel = Nothing
    }

unique :: CardDef -> CardDef
unique def = def {cdUnique = True}

doubleSided :: CardCode -> CardDef -> CardDef
doubleSided cCode def =
  def
    { cdDoubleSided = True
    , cdOtherSide = Just cCode
    }

-- | Printed-health helpers for the @cdHealth@ field, e.g. @cdHealth = health 1@.
health :: Int -> Maybe Health
health = Just . Health . Static

healthPerInvestigator :: Int -> Maybe Health
healthPerInvestigator = Just . Health . PerPlayer

healthX :: Maybe Health
healthX = Just (Health ValueX)

healthStar :: Maybe Health
healthStar = Just (Health ValueStar)

weakness :: CardCode -> Name -> CardDef
weakness cardCode name = baseEnemy cardCode name Nothing (Just Weakness)

basicWeakness :: CardCode -> Name -> CardDef
basicWeakness cardCode name =
  baseEnemy cardCode name Nothing (Just BasicWeakness)

enemy :: CardCode -> Name -> EncounterSet -> Int -> CardDef
enemy cardCode name encounterSet encounterSetQuantity =
  baseEnemy cardCode name (Just (encounterSet, encounterSetQuantity)) Nothing
