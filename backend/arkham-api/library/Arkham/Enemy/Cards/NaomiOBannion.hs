module Arkham.Enemy.Cards.NaomiOBannion (naomiOBannion) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyEvaded)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers
import Arkham.I18n (scope)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.MachinationsThroughTime.Helpers (scenarioI18n)

newtype NaomiOBannion = NaomiOBannion EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

naomiOBannion :: EnemyCard NaomiOBannion
naomiOBannion = enemy NaomiOBannion Cards.naomiOBannion

instance HasModifiersFor NaomiOBannion where
  getModifiersFor (NaomiOBannion attrs) = do
    n <- perPlayer 1
    modifySelf attrs [CannotMove, HealthModifier n]

instance HasAbilities NaomiOBannion where
  getAbilities (NaomiOBannion attrs) =
    extend1 attrs
      $ groupLimit PerRound
      $ mkAbility attrs 1
      $ forced
      $ EnemyEvaded #after Anyone (be attrs)

instance RunMessage NaomiOBannion where
  runMessage msg e@(NaomiOBannion attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      investigators <- select $ InvestigatorAt $ locationWithEnemy attrs.id
      if null investigators then readyThis attrs else forTargets investigators msg
      pure e
    ForTargets [] (UseThisAbility _ (isSource attrs -> True) 1) -> do
      readyThis attrs
      pure e
    ForTargets (InvestigatorTarget iid : rest) msg'@(UseThisAbility _ (isSource attrs -> True) 1) -> do
      chooseOneM iid $ scenarioI18n $ scope "naomiOBannion" do
        labeled' "takeDamage"
          $ assignDamage iid (attrs.ability 1) 1
        labeled' "doNotTakeDamage" $ forTargets rest msg'
      pure e
    _ -> NaomiOBannion <$> liftRunMessage msg attrs
