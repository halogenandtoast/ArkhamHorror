module Arkham.Types.Asset.Cards.SpiritSpeaker
  ( spiritSpeaker
  , SpiritSpeaker(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Uses
import Arkham.Types.AssetMatcher
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Target
import Arkham.Types.Window

newtype SpiritSpeaker = SpiritSpeaker AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spiritSpeaker :: AssetCard SpiritSpeaker
spiritSpeaker = asset SpiritSpeaker Cards.spiritSpeaker

instance HasSet AssetId env AssetMatcher => HasActions env SpiritSpeaker where
  getActions iid FastPlayerWindow (SpiritSpeaker attrs) = do
    targets <- getSet @AssetId (AssetOwnedBy iid <> AssetWithUseType Charge)
    pure
      [ UseAbility
          iid
          (mkAbility attrs 1 $ FastAbility $ ExhaustCost (toTarget attrs))
      | notNull targets
      ]
  getActions iid window (SpiritSpeaker attrs) = getActions iid window attrs

instance HasModifiersFor env SpiritSpeaker

instance
  ( HasCount UsesCount env AssetId
  , HasSet AssetId env AssetMatcher
  , HasQueue env
  , HasModifiersFor env ()
  )
  => RunMessage env SpiritSpeaker where
  runMessage msg a@(SpiritSpeaker attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      assetIds <- getSetList (AssetOwnedBy iid <> AssetWithUseType Charge)
      discardableAssetIds <- getSetList
        (AssetOwnedBy iid <> AssetWithUseType Charge <> DiscardableAsset)
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
