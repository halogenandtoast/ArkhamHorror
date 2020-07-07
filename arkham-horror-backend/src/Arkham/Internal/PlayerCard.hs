module Arkham.Internal.PlayerCard
  ( playerCardsInternal
  , toInternalPlayerCard
  , ArkhamPlayerCardInternal(..)
  , ArkhamPlayerCardType(..)
  )
where

import Arkham.Internal.Asset
import Arkham.Internal.Event
import Arkham.Types hiding (hand)
import Arkham.Types.Asset
import Arkham.Types.Card
import Arkham.Types.GameState
import Arkham.Types.Player
import Arkham.Types.Skill
import Arkham.Types.Trait
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
import Lens.Micro
import Lens.Micro.Platform ()

data ArkhamPlayerCardType = PlayerAsset | PlayerEvent | PlayerSkill | PlayerTreachery

playerCardsInternal :: HashMap ArkhamCardCode ArkhamPlayerCardInternal
playerCardsInternal =
  HashMap.map asset allAssets <> HashMap.map event allEvents

asset :: ArkhamAssetInternal -> ArkhamPlayerCardInternal
asset card@ArkhamAssetInternal {..} = ArkhamPlayerCardInternal
  { aciType = PlayerAsset
  , aciCost = Just assetCost
  , aciTraits = assetTraits
  , aciTestIcons = assetTestIcons
  , aciPlay = \g _ -> do
    asset' <- toAsset card
    pure $ g & activePlayer . assets %~ HashMap.insert (_assetId asset') asset'
  , aciActionCost = assetActionCost
  }

event :: ArkhamEventInternal -> ArkhamPlayerCardInternal
event card@ArkhamEventInternal {..} = ArkhamPlayerCardInternal
  { aciType = PlayerEvent
  , aciCost = Just eventCost
  , aciTraits = eventTraits
  , aciTestIcons = eventTestIcons
  , aciPlay = \g p ->
    eventAfterPlay g p
      <&> activePlayer
      . discard
      %~ (PlayerCard (eventToPlayerCard card) :)
  , aciActionCost = eventActionCost
  }

data ArkhamPlayerCardInternal = ArkhamPlayerCardInternal
  { aciType :: ArkhamPlayerCardType
  , aciCost :: Maybe Int
  , aciTraits :: HashSet ArkhamTrait
  , aciTestIcons :: [ArkhamSkillType]
  , aciPlay :: forall m. MonadIO m => ArkhamGameState -> ArkhamPlayer -> m ArkhamGameState
  , aciActionCost :: forall m. MonadIO m => ArkhamGameState -> ArkhamPlayer -> m Int
  }

toInternalPlayerCard :: ArkhamCard -> Maybe ArkhamPlayerCardInternal
toInternalPlayerCard c = HashMap.lookup (c ^. cardCode) playerCardsInternal

eventToPlayerCard :: ArkhamEventInternal -> ArkhamPlayerCard
eventToPlayerCard ArkhamEventInternal {..} = ArkhamPlayerCard
  { apcName = eventName
  , apcCost = Just eventCost
  , apcCode = eventCode
  , apcImage = eventImage
  , apcIsFast = eventIsFast
  , apcTestIcons = eventTestIcons
  }
