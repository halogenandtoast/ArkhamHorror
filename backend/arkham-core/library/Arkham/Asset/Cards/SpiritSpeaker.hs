module Arkham.Asset.Cards.SpiritSpeaker (
  spiritSpeaker,
  SpiritSpeaker (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher hiding (FastPlayerWindow)
import Arkham.Projection

newtype SpiritSpeaker = SpiritSpeaker AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spiritSpeaker :: AssetCard SpiritSpeaker
spiritSpeaker = asset SpiritSpeaker Cards.spiritSpeaker

instance HasAbilities SpiritSpeaker where
  getAbilities (SpiritSpeaker attrs) =
    [ restrictedAbility
        attrs
        1
        (ControlsThis <> AssetExists (AssetControlledBy You <> AssetWithUseType Charge))
        (FastAbility $ ExhaustCost $ toTarget attrs)
    ]

instance RunMessage SpiritSpeaker where
  runMessage msg a@(SpiritSpeaker attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      assetIds <- selectList $ AssetControlledBy You <> AssetWithUseType Charge
      discardableAssetIds <-
        selectList $ assetControlledBy iid <> AssetWithUseType Charge <> DiscardableAsset
      assetIdsWithChargeCounts <- forToSnd assetIds $ fieldMap AssetUses (findWithDefault 0 Charge)
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ TargetLabel
            target
            [ chooseOne
                player
                $ Label "Return to hand" [ReturnToHand iid target]
                : [ Label
                    "Move all charges to your resource pool"
                    [ SpendUses target Charge n
                    , TakeResources iid n (toAbilitySource attrs 1) False
                    , toDiscardBy iid (toAbilitySource attrs 1) target
                    ]
                  | aid `elem` discardableAssetIds
                  ]
            ]
          | (aid, n) <- assetIdsWithChargeCounts
          , let target = AssetTarget aid
          ]
      pure a
    _ -> SpiritSpeaker <$> runMessage msg attrs
