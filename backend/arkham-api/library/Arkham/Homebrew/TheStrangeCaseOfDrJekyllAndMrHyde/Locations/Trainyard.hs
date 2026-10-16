{- | Trainyard -- the end-of-round sweep.

The enemy damage is not conditional on anyone being here, so the Forced is
offered when either half has something to do; the tests are queued before the
damage so they resolve in printed order.
-}
module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Locations.Trainyard (trainyard) where

import Arkham.Ability
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype Trainyard = Trainyard LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trainyard :: LocationCard Trainyard
trainyard = location Trainyard Cards.trainyard 3 (PerPlayer 2)

instance HasAbilities Trainyard where
  getAbilities (Trainyard a) =
    extendRevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "trainyard.forced"
      $ skillTestAbility
      $ restricted
        a
        1
        (oneOf [exists (investigatorAt a), exists (NonEliteEnemy <> enemyAt a.id)])
      $ forced
      $ RoundEnds #when

instance RunMessage Trainyard where
  runMessage msg l@(Trainyard attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectEach (investigatorAt attrs) \iid -> do
        sid <- getRandom
        beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 3)
      selectEach (NonEliteEnemy <> enemyAt attrs.id)
        $ nonAttackEnemyDamage Nothing (attrs.ability 1) 1
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assignDamage iid (attrs.ability 1) 1
      pure l
    _ -> Trainyard <$> liftRunMessage msg attrs
