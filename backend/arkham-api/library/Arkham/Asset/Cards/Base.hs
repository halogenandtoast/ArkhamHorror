module Arkham.Asset.Cards.Base where

import Arkham.Asset.Uses hiding (Key, Lead)
import Arkham.Calculation
import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.Card.Cost
import Arkham.ChaosToken.Types qualified as Token
import Arkham.ClassSymbol
import Arkham.EncounterSet hiding (Dreamlands, Dunwich)
import Arkham.Id
import Arkham.Keyword (Keyword, Sealing (..))
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Name
import Arkham.Prelude

storyAsset :: CardCode -> Name -> Int -> EncounterSet -> CardDef
storyAsset cardCode name cost encounterSet =
  (baseAsset (Just (encounterSet, 1)) cardCode name cost (singleton Neutral)) {cdLevel = Nothing}

storyAssetWithMany :: CardCode -> Name -> Int -> EncounterSet -> Int -> CardDef
storyAssetWithMany cardCode name cost encounterSet encounterSetCount =
  (baseAsset (Just (encounterSet, encounterSetCount)) cardCode name cost (singleton Neutral))
    { cdLevel = Nothing
    }

asset :: CardCode -> Name -> Int -> ClassSymbol -> CardDef
asset cCode name cost classSymbol = baseAsset Nothing cCode name cost (singleton classSymbol)

multiClassAsset :: CardCode -> Name -> Int -> [ClassSymbol] -> CardDef
multiClassAsset cCode name cost classSymbols = baseAsset Nothing cCode name cost (setFromList classSymbols)

permanent :: CardDef -> CardDef
permanent cd = cd {cdPermanent = True, cdCost = Nothing}

fast :: CardDef -> CardDef
fast cd = cd {cdFastWindow = Just (DuringTurn You)}

weakness :: CardCode -> Name -> CardDef
weakness cardCode name =
  (baseAsset Nothing cardCode name 0 (singleton Neutral))
    { cdCardSubType = Just Weakness
    , cdRevelation = IsRevelation
    , cdCost = Nothing
    , cdLevel = Nothing
    }

basicWeakness :: CardCode -> Name -> CardDef
basicWeakness cardCode name =
  (baseAsset Nothing cardCode name 0 (singleton Neutral))
    { cdCardSubType = Just BasicWeakness
    , cdRevelation = IsRevelation
    , cdCost = Nothing
    , cdLevel = Nothing
    }

storyWeakness :: CardCode -> Name -> EncounterSet -> CardDef
storyWeakness cardCode name encounterSet =
  (baseAsset (Just (encounterSet, 1)) cardCode name 0 (singleton Neutral))
    { cdCardSubType = Just Weakness
    , cdRevelation = IsRevelation
    , cdCost = Nothing
    , cdLevel = Nothing
    }

uses :: UseType -> Int -> Uses GameCalculation
uses uType = Uses uType . Fixed

seal :: IsSealing s => s -> Keyword
seal (toSealing -> s) = Keyword.Seal s

class IsSealing s where
  toSealing :: s -> Sealing

instance IsSealing Sealing where
  toSealing = id
  {-# INLINE toSealing #-}

instance IsSealing ChaosTokenMatcher where
  toSealing s = Sealing s
  {-# INLINE toSealing #-}

instance IsSealing Token.ChaosTokenFace where
  toSealing = Sealing . ChaosTokenFaceIs
  {-# INLINE toSealing #-}

baseAsset
  :: Maybe (EncounterSet, Int)
  -> CardCode
  -> Name
  -> Int
  -> Set ClassSymbol
  -> CardDef
baseAsset mEncounterSet cardCode name cost classSymbols =
  (emptyCardDef cardCode name AssetType)
    { cdCost = Just (StaticCost cost)
    , cdClassSymbols = classSymbols
    , cdEncounterSet = fst <$> mEncounterSet
    , cdEncounterSetQuantity = snd <$> mEncounterSet
    }

signature :: InvestigatorId -> CardDef -> CardDef
signature iid cd = cd {cdDeckRestrictions = [Signature iid], cdLevel = Nothing}
