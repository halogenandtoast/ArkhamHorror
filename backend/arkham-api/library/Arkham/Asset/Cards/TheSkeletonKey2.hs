module Arkham.Asset.Cards.TheSkeletonKey2 (theSkeletonKey2, TheSkeletonKey2 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Placement

newtype TheSkeletonKey2 = TheSkeletonKey2 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSkeletonKey2 :: AssetCard TheSkeletonKey2
theSkeletonKey2 = asset TheSkeletonKey2 Cards.theSkeletonKey2

instance HasModifiersFor TheSkeletonKey2 where
  getModifiersFor (LocationTarget lid) (TheSkeletonKey2 a) =
    case a.placement of
      AttachedToLocation lid' | lid == lid' -> pure $ toModifiers a [SetShroud 1]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance HasAbilities TheSkeletonKey2 where
  getAbilities (TheSkeletonKey2 a) =
    [restrictedAbility a 1 (ControlsThis <> additionalCriteria) actionAbility]
   where
    additionalCriteria = case a.placement of
      AttachedToLocation _ -> OnSameLocation
      other -> if isInPlayArea other then (youExist $ at_ Anywhere) else Never

instance RunMessage TheSkeletonKey2 where
  runMessage msg a@(TheSkeletonKey2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      case attrs.placement of
        AttachedToLocation _ -> do
          push $ PlaceAsset (toId attrs) (InPlayArea iid)
        other | isInPlayArea other -> do
          lid <- getJustLocation iid
          push $ PlaceAsset (toId attrs) (AttachedToLocation lid)
        _ -> error "invalid placement"
      pure a
    _ -> TheSkeletonKey2 <$> liftRunMessage msg attrs
