module Arkham.Types.Asset.Cards.SpiritSpeaker
  ( spiritSpeaker
  , SpiritSpeaker(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Uses
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Restriction
import Arkham.Types.Target

newtype SpiritSpeaker = SpiritSpeaker AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spiritSpeaker :: AssetCard SpiritSpeaker
spiritSpeaker = asset SpiritSpeaker Cards.spiritSpeaker

instance HasActions SpiritSpeaker where
  getActions (SpiritSpeaker attrs) =
    [ restrictedAbility
        attrs
        1
        (OwnsThis <> AssetExists (AssetOwnedBy You <> AssetWithUseType Charge))
        (FastAbility ExhaustThis)
    ]

instance HasModifiersFor env SpiritSpeaker

instance
  ( HasCount UsesCount env AssetId
  , Query AssetMatcher env
  , HasQueue env
  , HasModifiersFor env ()
  )
  => RunMessage env SpiritSpeaker where
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
