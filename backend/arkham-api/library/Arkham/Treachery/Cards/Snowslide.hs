module Arkham.Treachery.Cards.Snowslide (snowslide) where

import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Snowslide = Snowslide TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

snowslide :: TreacheryCard Snowslide
snowslide = treachery Snowslide Cards.snowslide

instance RunMessage Snowslide where
  runMessage msg t@(Snowslide attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assets <- select $ assetControlledBy iid <> AssetWithHealth
      chooseOneAtATimeM iid do
        targeting iid $ directDamage iid attrs 1
        targets assets \aid -> dealAssetDirectDamage aid attrs 1
      pure t
    _ -> Snowslide <$> liftRunMessage msg attrs
