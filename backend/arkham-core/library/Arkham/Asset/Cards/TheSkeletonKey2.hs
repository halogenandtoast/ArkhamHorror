module Arkham.Asset.Cards.TheSkeletonKey2 (
  theSkeletonKey2,
  TheSkeletonKey2 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Helpers.Investigator
import Arkham.Placement

newtype TheSkeletonKey2 = TheSkeletonKey2 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSkeletonKey2 :: AssetCard TheSkeletonKey2
theSkeletonKey2 = asset TheSkeletonKey2 Cards.theSkeletonKey2

instance HasModifiersFor TheSkeletonKey2 where
  getModifiersFor (LocationTarget lid) (TheSkeletonKey2 a) =
    case assetPlacement a of
      AttachedToLocation lid'
        | lid == lid' ->
            pure $ toModifiers a [SetShroud 1]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance HasAbilities TheSkeletonKey2 where
  getAbilities (TheSkeletonKey2 a) =
    [ restrictedAbility a 1 (ControlsThis <> additionalCriteria) $
        ActionAbility Nothing $
          ActionCost 1
    ]
   where
    additionalCriteria = case assetPlacement a of
      InPlayArea _ -> NoRestriction
      AttachedToLocation _ -> OnSameLocation
      _ -> Never

instance RunMessage TheSkeletonKey2 where
  runMessage msg a@(TheSkeletonKey2 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      case assetPlacement attrs of
        InPlayArea _ -> do
          lid <- getJustLocation iid
          push $ PlaceAsset (toId attrs) (AttachedToLocation lid)
        AttachedToLocation _ -> do
          push $ PlaceAsset (toId attrs) (InPlayArea iid)
        _ -> error "invalid placement"
      pure a
    _ -> TheSkeletonKey2 <$> runMessage msg attrs
