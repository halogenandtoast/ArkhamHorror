module Arkham.Internal.Asset
  ( ArkhamAssetInternal(..)
  , allAssets
  , toAsset
  )
where

import Arkham.Types.Asset
import Arkham.Types.Card
import Arkham.Types.GameState
import Arkham.Types.Player
import Arkham.Types.Skill
import Arkham.Types.Trait
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
import Data.UUID.V4

data SlotType = Accessory | Body | Hand | Arcane | Ally | DoubleHand | DoubleArcane

data ArkhamAssetInternal = ArkhamAssetInternal
  { assetName :: Text
  , assetCode :: ArkhamCardCode
  , assetImage :: Text
  , assetCost :: Int
  , assetTraits :: HashSet ArkhamTrait
  , assetTestIcons :: [ArkhamSkillType]
  , assetSlots :: [SlotType]
  , assetHealth :: Maybe Int
  , assetSanity :: Maybe Int
  , assetUses :: Maybe Int
  , assetActionCost :: forall m. MonadIO m => ArkhamGameState -> ArkhamPlayer -> m Int
  }

allAssets :: HashMap ArkhamCardCode ArkhamAssetInternal
allAssets = HashMap.fromList $ map
  (\a -> (assetCode a, a))
  [ fortyFiveAutomatic
  , guardDog
  , rolands38Special
  , physicalTraining
  , flashlight
  , knife
  , machete
  ]

toAsset :: MonadIO m => ArkhamAssetInternal -> m ArkhamAsset
toAsset ArkhamAssetInternal {..} = do
  assetId <- liftIO nextRandom
  pure $ ArkhamAsset
    { _assetName = assetName
    , _assetCost = assetCost
    , _assetCode = assetCode
    , _assetImage = assetImage
    , _assetUses = assetUses
    , _assetHasActionsAvailable = False
    , _assetId = assetId
    }

asset :: Text -> ArkhamCardCode -> Int -> ArkhamAssetInternal
asset name code cost = ArkhamAssetInternal
  { assetCost = cost
  , assetTraits = mempty
  , assetTestIcons = []
  , assetActionCost = const (const (pure 1))
  , assetSlots = []
  , assetName = name
  , assetCode = code
  , assetImage =
    "https://arkhamdb.com/bundles/cards/" <> unArkhamCardCode code <> ".png"
  , assetHealth = Nothing
  , assetSanity = Nothing
  , assetUses = Nothing
  }

hand :: Text -> ArkhamCardCode -> Int -> ArkhamAssetInternal
hand name code cost = (asset name code cost) { assetSlots = [Hand] }

ally :: Text -> ArkhamCardCode -> Int -> Int -> Int -> ArkhamAssetInternal
ally name code cost health sanity = (asset name code cost)
  { assetSlots = [Ally]
  , assetHealth = Just health
  , assetSanity = Just sanity
  }

-- hasUsesRemaining :: ArkhamAsset -> Bool
-- hasUsesRemaining = view (uses . non 0 . to (> 0))

withUses :: Int -> ArkhamAssetInternal -> ArkhamAssetInternal
withUses uses' c = c { assetUses = Just uses' }

fortyFiveAutomatic :: ArkhamAssetInternal
fortyFiveAutomatic = withUses 4
  $ (hand ".45 Automatic" "01016" 4) { assetTestIcons = [ArkhamSkillAgility] }

guardDog :: ArkhamAssetInternal
guardDog =
  (ally "Guard Dog" "01021" 3 3 1) { assetTestIcons = [ArkhamSkillCombat] }

rolands38Special :: ArkhamAssetInternal
rolands38Special = withUses 4 $ (hand "Roland's .38 Special" "01006" 3)
  { assetTestIcons = [ArkhamSkillCombat, ArkhamSkillAgility, ArkhamSkillWild]
  }

physicalTraining :: ArkhamAssetInternal
physicalTraining = (asset "Physical Training" "01017" 2)
  { assetTestIcons = [ArkhamSkillWillpower, ArkhamSkillCombat]
  }

flashlight :: ArkhamAssetInternal
flashlight = withUses 3
  $ (hand "Flashlight" "01087" 2) { assetTestIcons = [ArkhamSkillIntellect] }

knife :: ArkhamAssetInternal
knife = (hand "Knife" "01086" 1) { assetTestIcons = [ArkhamSkillCombat] }

machete :: ArkhamAssetInternal
machete = (hand "Machete" "01020" 3) { assetTestIcons = [ArkhamSkillCombat] }
