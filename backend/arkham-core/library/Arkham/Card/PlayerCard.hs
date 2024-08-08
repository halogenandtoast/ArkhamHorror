{-# LANGUAGE TemplateHaskell #-}

module Arkham.Card.PlayerCard where

import Arkham.Prelude

import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.Class
import Arkham.Card.Cost
import Arkham.Card.Id
import Arkham.Customization
import Arkham.Enemy.Cards (allSpecialEnemyCards)
import Arkham.Id
import Arkham.Json
import Arkham.Name
import Arkham.PlayerCard
import Arkham.SkillType
import Arkham.Taboo.Types
import Data.Aeson.TH
import GHC.Records

data PlayerCard = MkPlayerCard
  { pcId :: CardId
  , pcOwner :: Maybe InvestigatorId
  , pcCardCode :: CardCode
  , pcOriginalCardCode :: CardCode
  , pcCustomizations :: Customizations
  , pcTabooList :: Maybe TabooList
  , pcMutated :: Maybe Text
  }
  deriving stock (Show, Ord, Data)

instance HasField "id" PlayerCard CardId where
  getField = pcId

instance HasField "customizations" PlayerCard Customizations where
  getField = pcCustomizations

instance HasField "owner" PlayerCard (Maybe InvestigatorId) where
  getField = pcOwner

instance Eq PlayerCard where
  pc1 == pc2 = pcId pc1 == pcId pc2

instance HasCardCode PlayerCard where
  toCardCode = pcCardCode

instance HasCost PlayerCard where
  getCost c = case cdCost (toCardDef c) of
    Just (StaticCost n) -> n
    Just DynamicCost -> 0
    Just (MaxDynamicCost _) -> 0
    Just DiscardAmountCost -> 0
    Nothing -> 0

instance HasField "skills" PlayerCard [SkillIcon] where
  getField = cdSkills . toCardDef

instance HasCardDef PlayerCard where
  toCardDef c = case lookup (pcCardCode c) (allPlayerCards <> allSpecialEnemyCards) of
    Just def -> def
    Nothing ->
      error $ "missing card def for player card " <> show (pcCardCode c)

instance Named PlayerCard where
  toName = toName . toCardDef

instance HasOriginalCardCode PlayerCard where
  toOriginalCardCode = pcOriginalCardCode

lookupPlayerCard :: CardDef -> CardId -> PlayerCard
lookupPlayerCard cardDef cardId =
  MkPlayerCard
    { pcId = cardId
    , pcCardCode = toCardCode cardDef
    , pcOriginalCardCode = toCardCode cardDef
    , pcOwner = Nothing
    , pcCustomizations = mempty
    , pcTabooList = Nothing
    , pcMutated = Nothing
    }

setPlayerCardOwner :: InvestigatorId -> PlayerCard -> PlayerCard
setPlayerCardOwner iid pc = pc {pcOwner = Just iid}

setTaboo :: Maybe TabooList -> PlayerCard -> PlayerCard
setTaboo mtaboo pc = pc {pcTabooList = mtaboo, pcMutated = tabooMutated mtaboo pc}

tabooMutated :: Maybe TabooList -> PlayerCard -> Maybe Text
tabooMutated Nothing _ = Nothing
tabooMutated jtbl pc = tabooMutated' jtbl (pcCardCode pc)

tabooMutated' :: Maybe TabooList -> CardCode -> Maybe Text
tabooMutated' = \case
  Just TabooList15 -> tabooMutated15
  Just TabooList16 -> tabooMutated16
  Just TabooList18 -> tabooMutated18
  Just TabooList19 -> tabooMutated19
  Just TabooList20 -> tabooMutated20
  Just TabooList21 -> tabooMutated21
  Just TabooList22 -> tabooMutated22
  _ -> \_ -> Nothing

tabooMutated15 :: CardCode -> Maybe Text
tabooMutated15 = \case
  "01033" -> Just "Mutated15"
  "02002" -> Just "Mutated15"
  "02111" -> Just "Mutated15"
  "02229" -> Just "Mutated15"
  "02266" -> Just "Mutated15"
  "03029" -> Just "Mutated15"
  "03315" -> Just "Mutated15"
  _ -> Nothing

tabooMutated16 :: CardCode -> Maybe Text
tabooMutated16 = \case
  pc -> tabooMutated15 pc

tabooMutated18 :: CardCode -> Maybe Text
tabooMutated18 = \case
  "02229" -> Just "Mutated18"
  "03029" -> Just "Mutated18"
  "04309" -> Just "Mutated18"
  "05116" -> Just "Mutated18"
  "05188" -> Just "Mutated18"
  "05189" -> Just "Mutated18"
  "06195" -> Just "Mutated18"
  pc -> tabooMutated16 pc

tabooMutated19 :: CardCode -> Maybe Text
tabooMutated19 = \case
  "01050" -> Just "Mutated19"
  "02226" -> Just "Mutated19"
  "05230" -> Just "Mutated19"
  "05324" -> Just "Mutated19"
  "06022" -> Just "Mutated19"
  "06199" -> Just "Mutated19"
  "07269" -> Just "Mutated19"
  "07301" -> Just "Mutated19"
  pc -> tabooMutated18 pc

tabooMutated20 :: CardCode -> Maybe Text
tabooMutated20 = \case
  "02029" -> Just "Mutated20"
  "02269" -> Just "Mutated20"
  "03006" -> Just "Mutated20"
  "03019" -> Just "Mutated20"
  "04023" -> Just "Mutated20"
  "04110" -> Just "Mutated20"
  "05153" -> Just "Mutated20"
  "06002" -> Just "Mutated20"
  "08055" -> Just "Mutated20"
  "08093" -> Just "Mutated20"
  "08098" -> Just "Mutated20"
  "08100" -> Just "Mutated20"
  "07268" -> Just "Mutated20"
  "60405" -> Just "Mutated20"
  "60416" -> Just "Mutated20"
  "60417" -> Just "Mutated20"
  "60423" -> Just "Mutated20"
  pc -> tabooMutated19 pc

tabooMutated21 :: CardCode -> Maybe Text
tabooMutated21 = \case
  "03112" -> Just "Mutated21"
  "04105" -> Just "Mutated21"
  "05020" -> Just "Mutated21"
  "05113" -> Just "Mutated21"
  "07003" -> Just "Mutated21"
  "08019" -> Just "Mutated21"
  "08032" -> Just "Mutated21"
  "08076" -> Just "Mutated21"
  "09045" -> Just "Mutated21"
  "09081" -> Just "Mutated21"
  "60318" -> Just "Mutated21"
  pc -> tabooMutated20 pc

tabooMutated22 :: CardCode -> Maybe Text
tabooMutated22 = \case
  "02153" -> Just "Mutated22"
  "04233" -> Just "Mutated22"
  "07122" -> Just "Mutated22"
  "60414" -> Just "Mutated22"
  pc -> tabooMutated21 pc

$(deriveJSON (aesonOptions $ Just "pc") ''PlayerCard)
