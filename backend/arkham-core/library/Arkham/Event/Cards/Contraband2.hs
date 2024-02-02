module Arkham.Event.Cards.Contraband2 (
  contraband2,
  Contraband2 (..),
) where

import Arkham.Prelude

import Arkham.Asset.Types (Field (..))
import Arkham.Asset.Uses
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher

newtype Contraband2 = Contraband2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

contraband2 :: EventCard Contraband2
contraband2 = event Contraband2 Cards.contraband2

instance RunMessage Contraband2 where
  runMessage msg e@(Contraband2 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      investigatorIds <- selectList $ affectsOthers $ colocatedWith iid

      ammoAssets <-
        selectWithField AssetUses
          $ AssetWithUseType Ammo
          <> AssetNotAtUseLimit
          <> AssetOneOf
            (map assetControlledBy investigatorIds)

      supplyAssets <-
        selectWithField AssetUses
          $ AssetWithUseType Supply
          <> AssetNotAtUseLimit
          <> AssetOneOf
            (map assetControlledBy investigatorIds)

      let
        ammoAssetsWithUseCount =
          map (\(aid, uses) -> (Ammo, findWithDefault 0 Ammo uses, aid)) ammoAssets
        supplyAssetsWithUseCount =
          map (\(aid, uses) -> (Supply, findWithDefault 0 Supply uses, aid)) supplyAssets

      drawing <- drawCards iid attrs 1
      player <- getPlayer iid

      push
        $ chooseOne
          player
          [ Label
              "Place 2 ammo or supply tokens on that asset and draw 1 card."
              [ chooseOne
                  player
                  [ targetLabel assetId [AddUses assetId useType' 2, drawing]
                  | (useType', _, assetId) <-
                      ammoAssetsWithUseCount <> supplyAssetsWithUseCount
                  ]
              ]
          , Label
              "Double the number of ammo or supply tokens on that asset."
              [ chooseOne
                  player
                  [ targetLabel assetId [AddUses assetId useType' assetUseCount]
                  | (useType', assetUseCount, assetId) <-
                      ammoAssetsWithUseCount <> supplyAssetsWithUseCount
                  ]
              ]
          ]
      pure e
    _ -> Contraband2 <$> runMessage msg attrs
