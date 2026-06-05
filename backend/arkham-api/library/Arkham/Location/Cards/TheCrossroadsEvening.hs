module Arkham.Location.Cards.TheCrossroadsEvening (theCrossroadsEvening) where

import Arkham.Ability
import Arkham.Helpers.SkillTest.Lifted
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Resident))

newtype TheCrossroadsEvening = TheCrossroadsEvening LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCrossroadsEvening :: LocationCard TheCrossroadsEvening
theCrossroadsEvening =
  symbolLabel
    $ location TheCrossroadsEvening Cards.theCrossroadsEvening 3 (Static 0)

instance HasAbilities TheCrossroadsEvening where
  getAbilities (TheCrossroadsEvening a) =
    extendRevealed1 a
      $ skillTestAbility
      $ restricted
        a
        1
        ( Here
            <> youExist InvestigatorWithAnyDamage
            <> exists (EnemyAt (be a) <> withTrait Resident <> EnemyCanBeDamagedBySource (a.ability 1))
        )
        parleyAction_

instance RunMessage TheCrossroadsEvening where
  runMessage msg l@(TheCrossroadsEvening attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) iid #willpower (Fixed 3)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      whenM (iid <=~> InvestigatorWithAnyDamage) do
        enemies <-
          select $ EnemyAt (be attrs) <> withTrait Resident <> EnemyCanBeDamagedBySource (attrs.ability 1)
        chooseTargetM iid enemies \enemy -> do
          healDamage iid (attrs.ability 1) 1
          nonAttackEnemyDamage (Just iid) (attrs.ability 1) 1 enemy
      pure l
    _ -> TheCrossroadsEvening <$> liftRunMessage msg attrs
