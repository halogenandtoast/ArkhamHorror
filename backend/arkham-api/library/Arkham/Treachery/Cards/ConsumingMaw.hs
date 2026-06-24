module Arkham.Treachery.Cards.ConsumingMaw (consumingMaw) where

import Arkham.Ability
import Arkham.Helpers.Window (getDefeatedAsset)
import Arkham.Matcher hiding (AssetCard)
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted hiding (AssetDefeated)

newtype ConsumingMaw = ConsumingMaw TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

consumingMaw :: TreacheryCard ConsumingMaw
consumingMaw = treachery ConsumingMaw Cards.consumingMaw

instance HasAbilities ConsumingMaw where
  getAbilities (ConsumingMaw a) =
    [mkAbility a 1 $ SilentForcedAbility $ AssetDefeated #when (BySource $ SourceIs $ toSource a) AnyAsset]

instance RunMessage ConsumingMaw where
  runMessage msg t@(ConsumingMaw attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 3)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n | n > 0 -> do
      assignDamage iid attrs n
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 (getDefeatedAsset -> aid) _ -> do
      scenarioSpecific "devour" (toTarget aid)
      pure t
    _ -> ConsumingMaw <$> liftRunMessage msg attrs
