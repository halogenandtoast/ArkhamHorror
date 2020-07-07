module Arkham.Internal.Asset where

import Arkham.Types.Card
import Arkham.Types.Skill
import Arkham.Types.Trait
import ClassyPrelude

data SlotType = Accessory | Body | Hand | Arcane | Ally | DoubleHand | DoubleArcane

data ArkhamAssetInternal = ArkhamAssetInternal
  { assetCost :: Int
  , assetTraits :: HashSet ArkhamTrait
  , assetTestIcons :: [ArkhamSkillType]
  , assetActionCost :: Int
  , assetSlots :: [SlotType]
  , assetHealth :: Maybe Int
  , assetSanity :: Maybe Int
  , assetUses :: Maybe Int
  }

allAssets :: HashMap ArkhamCardCode ArkhamAssetInternal
allAssets = undefined

asset :: Int -> ArkhamAssetInternal
asset cost = ArkhamAssetInternal
  { assetCost = cost
  , assetTraits = mempty
  , assetTestIcons = []
  , assetActionCost = 1
  , assetSlots = []
  }

hand :: Int -> ArkhamAssetInternal
hand cost = (asset cost) { assetSlots = [Hand] }

ally :: Int -> Int -> Int -> ArkhamAssetInternal
ally cost health sanity = (asset cost)
  { assetSlots = [Ally]
  , assetHealth = Just health
  , assetSanity = Just sanity
  }

-- hasUsesRemaining :: ArkhamAsset -> Bool
-- hasUsesRemaining = view (uses . non 0 . to (> 0))

withUses :: Int -> ArkhamAssetInternal -> ArkhamAssetInternal
withUses uses' c = c { assetUses = uses' }

fortyFiveAutomatic :: ArkhamAssetInternal
fortyFiveAutomatic = withUses 4 $ (hand 4) { assetTestIcons = [agility] }

guardDog :: ArkhamAssetInternal
guardDog = (ally 3 3 1) { assetTestIcons = [combat] }

rolands38Special :: ArkhamAssetInternal
rolands38Special =
  withUses 4 $ (hand 3) { assetTestIcons = [combat, agility, wild] }

physicalTraining :: ArkhamAssetInternal
physicalTraining = (asset 2) { assetTestIcons = [willpower, combat] }

flashlight :: ArkhamAssetInternal
flashlight = withUses 3 $ (hand 2) { assetTestIcons = [intellect] }

knife :: ArkhamAssetInternal
knife = (hand 1) { assetTestIcons = [combat] }

machete :: ArkhamAssetInternal
machete = (hand 3) { assetTestIcons = [combat] }
