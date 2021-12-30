module Arkham.Asset.Cards.SpiritSpeaker
  ( spiritSpeaker
  , SpiritSpeaker(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Attrs
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher hiding (FastPlayerWindow)
import Arkham.Query
import Arkham.Target

newtype SpiritSpeaker = SpiritSpeaker AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spiritSpeaker :: AssetCard SpiritSpeaker
spiritSpeaker = asset SpiritSpeaker Cards.spiritSpeaker

instance HasAbilities SpiritSpeaker where
  getAbilities (SpiritSpeaker attrs) =
    [ restrictedAbility
        attrs
        1
        (OwnsThis <> AssetExists (AssetOwnedBy You <> AssetWithUseType Charge))
        (FastAbility $ ExhaustCost $ toTarget attrs)
    ]

instance AssetRunner env => RunMessage env SpiritSpeaker where
  runMessage msg a@(SpiritSpeaker attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      assetIds <- selectList (AssetOwnedBy You <> AssetWithUseType Charge)
      discardableAssetIds <- selectList
        (AssetOwnedBy You <> AssetWithUseType Charge <> DiscardableAsset)
      assetIdsWithChargeCounts <- traverse
        (traverseToSnd (fmap unUsesCount . getCount))
        assetIds
      a <$ push
        (chooseOne
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
        )
    _ -> SpiritSpeaker <$> runMessage msg attrs
