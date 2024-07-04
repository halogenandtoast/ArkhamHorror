module Arkham.Treachery.Cards.Punishment (punishment, Punishment (..)) where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getSkillTestInvestigator, getSkillTestSource)
import Arkham.Matcher
import Arkham.Source
import Arkham.Trait (Trait (Witch))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted hiding (EnemyDefeated)

newtype Punishment = Punishment TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

punishment :: TreacheryCard Punishment
punishment = treachery Punishment Cards.punishment

instance HasModifiersFor Punishment where
  getModifiersFor (InvestigatorTarget iid) (Punishment attrs) = do
    maybeModified attrs do
      source <- MaybeT getSkillTestSource
      investigator <- MaybeT getSkillTestInvestigator
      guard $ isSource attrs source && iid == investigator
      guardM . lift . selectAny $ ExhaustedEnemy <> EnemyWithTrait Witch <> enemyAtLocationWith iid
      pure [SkillTestAutomaticallySucceeds]
  getModifiersFor _ _ = pure []

instance HasAbilities Punishment where
  getAbilities (Punishment a) =
    [restrictedAbility a 1 (InThreatAreaOf You) $ forced $ EnemyDefeated #after Anyone ByAny AnyEnemy]

instance RunMessage Punishment where
  runMessage msg t@(Punishment attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      beginSkillTest iid (attrs.ability 2) attrs #willpower (Fixed 3)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> Punishment <$> liftRunMessage msg attrs
