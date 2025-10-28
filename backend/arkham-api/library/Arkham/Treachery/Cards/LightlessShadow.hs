module Arkham.Treachery.Cards.LightlessShadow (lightlessShadow) where

import Arkham.ScenarioLogKey
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype LightlessShadow = LightlessShadow TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lightlessShadow :: TreacheryCard LightlessShadow
lightlessShadow = treachery LightlessShadow Cards.lightlessShadow

instance RunMessage LightlessShadow where
  runMessage msg t@(LightlessShadow attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility
        $ SumCalculation [Fixed 1, ScenarioCount CurrentDepth]
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignDamage iid attrs 2
      pure t
    _ -> LightlessShadow <$> liftRunMessage msg attrs
