module Arkham.Location.Cards.SaadiansTombsAbandoned (saadiansTombsAbandoned) where

import Arkham.Ability
import Arkham.Helpers.Window (getEnemy)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype SaadiansTombsAbandoned = SaadiansTombsAbandoned LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

saadiansTombsAbandoned :: LocationCard SaadiansTombsAbandoned
saadiansTombsAbandoned = symbolLabel $ location SaadiansTombsAbandoned Cards.saadiansTombsAbandoned 5 (Static 0)

instance HasAbilities SaadiansTombsAbandoned where
  getAbilities (SaadiansTombsAbandoned a) =
    extendRevealed1 a
      $ mkAbility a 1
      $ forced
      $ EnemyEnters #when (be a) (NonEliteEnemy <> EnemyWithAnyDamage)

instance RunMessage SaadiansTombsAbandoned where
  runMessage msg l@(SaadiansTombsAbandoned attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (getEnemy -> enemy) _ -> do
      healAllDamage (attrs.ability 1) enemy
      pure l
    _ -> SaadiansTombsAbandoned <$> liftRunMessage msg attrs
