module Arkham.Treachery.Cards.Bedeviled (
  bedeviled,
  Bedeviled (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Source
import Arkham.Trait (Trait (Witch))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype Bedeviled = Bedeviled TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bedeviled :: TreacheryCard Bedeviled
bedeviled = treachery Bedeviled Cards.bedeviled

instance HasModifiersFor Bedeviled where
  getModifiersFor (InvestigatorTarget iid) (Bedeviled attrs) | treacheryOnInvestigator iid attrs = do
    skillTestModifiers' <- runMaybeT $ do
      source <- MaybeT getSkillTestSource
      investigator <- MaybeT getSkillTestInvestigator
      guard $ isSource attrs source && iid == investigator
      guardM
        $ lift
        $ selectAny
        $ ExhaustedEnemy
        <> EnemyWithTrait Witch
        <> EnemyAt (locationWithInvestigator iid)
      pure SkillTestAutomaticallySucceeds
    pure
      $ toModifiers attrs
      $ CannotTriggerAbilityMatching
        (AbilityIsActionAbility <> AbilityOnCardControlledBy iid)
      : maybeToList skillTestModifiers'
  getModifiersFor _ _ = pure []

instance HasAbilities Bedeviled where
  getAbilities (Bedeviled a) =
    [restrictedAbility a 1 OnSameLocation $ ActionAbility Nothing $ ActionCost 1]

instance RunMessage Bedeviled where
  runMessage msg t@(Bedeviled attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ AttachTreachery (toId attrs) (toTarget iid)
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ RevelationSkillTest iid (toSource attrs) SkillWillpower 3
      pure t
    PassedSkillTest _ _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ _ -> do
      push $ Discard (toAbilitySource attrs 1) (toTarget attrs)
      pure t
    _ -> Bedeviled <$> runMessage msg attrs
