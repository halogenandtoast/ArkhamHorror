module Arkham.Event.Cards.Contraband (
  contraband,
  Contraband (..),
) where

import Arkham.Prelude

import Arkham.Asset.Types
import Arkham.Asset.Uses
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Projection

newtype Contraband = Contraband EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

contraband :: EventCard Contraband
contraband = event Contraband Cards.contraband

instance RunMessage Contraband where
  runMessage msg e@(Contraband attrs@EventAttrs {..}) = case msg of
    PlayThisEvent iid eid | eid == eventId -> do
      investigatorIds <- selectList =<< guardAffectsColocated iid

      ammoAssets <-
        selectList
          $ AssetWithUseType Ammo
          <> AssetNotAtUseLimit
          <> oneOf (map assetControlledBy investigatorIds)

      ammoAssetsWithUseCount <-
        map (\(c, aid) -> (Ammo, c, aid))
          <$> for
            ammoAssets
            (\aid -> (,aid) <$> fieldMap AssetUses (findWithDefault 0 Ammo) aid)

      supplyAssets <-
        selectList
          $ AssetWithUseType Supply
          <> AssetNotAtUseLimit
          <> oneOf (map assetControlledBy investigatorIds)

      supplyAssetsWithUseCount <-
        map (\(c, aid) -> (Supply, c, aid))
          <$> for
            supplyAssets
            (\aid -> (,aid) <$> fieldMap AssetUses (findWithDefault 0 Supply) aid)

      player <- getPlayer iid
      pushAll
        [ chooseOne
            player
            [ targetLabel assetId [AddUses assetId useType' assetUseCount]
            | (useType', assetUseCount, assetId) <- ammoAssetsWithUseCount <> supplyAssetsWithUseCount
            ]
        ]
      pure e
    _ -> Contraband <$> runMessage msg attrs
