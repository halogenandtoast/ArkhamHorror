module Arkham.Internal.PlayerCard
  ( playerCardsInternal
  , toInternalPlayerCard
  , ArkhamPlayerCardInternal(..)
  , ArkhamPlayerCardType(..)
  )
where

import Arkham.Internal.Location
import Arkham.Types hiding (hand)
import Arkham.Types.Card
import Arkham.Types.GameState
import Arkham.Types.Location
import Arkham.Types.Skill
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
import Lens.Micro
import Lens.Micro.Extras
import Lens.Micro.Platform ()

data ArkhamPlayerCardType = PlayerAsset | PlayerEvent | PlayerSkill | PlayerTreachery

playerCardsInternal :: HashMap ArkhamCardCode ArkhamPlayerCardInternal
playerCardsInternal = HashMap.map asset allAssets

asset :: ArkhamAssetInternal -> ArkhamPlayerCardInternal
asset card@ArkhamAssetInternal {..} = ArkhamPlayerCardInternal
  { aciType = PlayerAsset
  , aciCost = assetCost
  , aciTraits = assetTraits
  , aciTestIcons = assetTestIcons
  , aciPlay = \g p -> do
    assetId <- liftIO nextRandom
    g ^. activePlayer . assets %~ HashMap.insert assetId (toAsset card)
  , aciActionCost = assetActionCost
  }

data ArkhamPlayerCardInternal = ArkhamPlayerCardInternal
  { aciType :: ArkhamPlayerCardType
  , aciCost :: Maybe Int
  , aciTraits :: HashSet ArkhamTrait
  , aciTestIcons :: [ArkhamSkillType]
  , aciPlay :: ArkhamGameState -> ArkhamCard -> ArkhamCard
  , aciActionCost :: ArkhamGameState -> Int
  }

card :: Int -> ArkhamPlayerCardType -> ArkhamPlayerCardInternal
card cost cardType = ArkhamPlayerCardInternal
  { aciType = cardType
  , aciCost = Just cost
  , aciTraits = mempty
  , aciSlots = []
  , aciTestIcons = []
  , aciPlay = flip const
  , aciActionCost = const 1
  }

fast :: ArkhamPlayerCardInternal -> ArkhamPlayerCardInternal
fast c = c { aciActionCost = const 0 }

toInternalPlayerCard :: ArkhamCard -> Maybe ArkhamPlayerCardInternal
toInternalPlayerCard c = HashMap.lookup (c ^. cardCode) playerCardsInternal

playerCardsInternal :: HashMap ArkhamCardCode ArkhamPlayerCardInternal
playerCardsInternal = HashMap.fromList
  [ ("01006", rolands38Special)
  , ("01007", coverUp)
  , ("01016", fortyFiveAutomatic)
  , ("01017", physicalTraining)
  , ("01020", machete)
  , ("01021", guardDog)
  , ("01022", evidence)
  , ("01023", dodge)
  , ("01025", viciousBlow)
  , ("01023", dynamiteBlast)
  , ("01037", workingAHunch)
  , ("01039", deduction)
  , ("01086", knife)
  , ("01087", flashlight)
  , ("01088", emergencyCache)
  , ("01089", guts)
  , ("01091", overpower)
  , ("01093", unexpectedCourage)
  ]
