module Arkham.Asset.Cards.SpiritSpeaker
  ( spiritSpeaker
  , SpiritSpeaker(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher hiding (FastPlayerWindow)
import Arkham.Projection
import Arkham.Target

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
      assetIds <- selectList (AssetControlledBy You <> AssetWithUseType Charge)
      discardableAssetIds <- selectList
        (AssetControlledBy You <> AssetWithUseType Charge <> DiscardableAsset)
      assetIdsWithChargeCounts <- traverse
        (traverseToSnd (fmap useCount . field AssetUses))
        assetIds
      push $
        chooseOne
          iid
          [ TargetLabel
              target
              [ chooseOne
                  iid
                  (Label "Return to hand" [ReturnToHand iid target]
                  : [ Label
                        "Move all charges to your resource pool"
                        [ SpendUses target Charge n
                        , TakeResources iid n False
                        , Discard target
                        ]
                    | aid `elem` discardableAssetIds
                    ]
                  )
              ]
          | (aid, n) <- assetIdsWithChargeCounts
          , let target = AssetTarget aid
          ]
      pure a
    _ -> SpiritSpeaker <$> runMessage msg attrs
