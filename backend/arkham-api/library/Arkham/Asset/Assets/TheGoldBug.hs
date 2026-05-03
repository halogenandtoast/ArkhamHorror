module Arkham.Asset.Assets.TheGoldBug (theGoldBug) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), inThreatAreaGets)
import Arkham.Message.Lifted.Placement

newtype TheGoldBug = TheGoldBug AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGoldBug :: AssetCard TheGoldBug
theGoldBug = assetWith TheGoldBug Cards.theGoldBug (canLeavePlayByNormalMeansL .~ False)

instance HasAbilities TheGoldBug where
  getAbilities (TheGoldBug a) = [restricted a 1 OnSameLocation actionAbility]

instance HasModifiersFor TheGoldBug where
  getModifiersFor (TheGoldBug a) = inThreatAreaGets a [SanityModifier (-1), HealthModifier (-1)]

instance RunMessage TheGoldBug where
  runMessage msg a@(TheGoldBug attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      place attrs (InThreatArea iid)
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      shuffleIntoDeck iid attrs
      pure a
    _ -> TheGoldBug <$> liftRunMessage msg attrs
