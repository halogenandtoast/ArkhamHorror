module Arkham.Treachery.Cards.ConsumingMaw (consumingMaw) where

import Arkham.Ability
import Arkham.Asset.Types (Field (..))
import Arkham.Helpers.Window (getDefeatedAsset)
import Arkham.Matcher hiding (AssetCard)
import Arkham.Projection
import Arkham.Scenarios.TheBlobThatAteEverything.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted hiding (AssetDefeated)

newtype ConsumingMaw = ConsumingMaw TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

consumingMaw :: TreacheryCard ConsumingMaw
consumingMaw = treachery ConsumingMaw Cards.consumingMaw

instance HasAbilities ConsumingMaw where
  getAbilities (ConsumingMaw a) =
    [mkAbility a 1 $ forced $ AssetDefeated #after (BySource $ SourceIs $ toSource a) AnyAsset]

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
      card <- field AssetCard aid
      devour [card]
      pure t
    _ -> ConsumingMaw <$> liftRunMessage msg attrs
