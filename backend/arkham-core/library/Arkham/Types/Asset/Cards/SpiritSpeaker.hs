module Arkham.Types.Asset.Cards.SpiritSpeaker
  ( spiritSpeaker
  , SpiritSpeaker(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Matcher hiding (FastPlayerWindow)
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Target

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
