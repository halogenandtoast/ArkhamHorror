module Arkham.Treachery.Cards.Bedeviled (bedeviled, Bedeviled (..)) where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getSkillTest, getSkillTestInvestigator, getSkillTestSource)
import Arkham.Matcher
import Arkham.Placement
import Arkham.Source
import Arkham.Trait (Trait (Witch))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Bedeviled = Bedeviled TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bedeviled :: TreacheryCard Bedeviled
bedeviled = treachery Bedeviled Cards.bedeviled

instance HasModifiersFor Bedeviled where
  getModifiersFor (Bedeviled a) = do
    threatArea <- case a.placement of
      InThreatArea iid -> do
        inThreatAreaGets
          a
          [CannotTriggerAbilityMatching (AbilityIsActionAbility <> AbilityOnCardControlledBy iid)]
      _ -> pure mempty
    skillTest <-
      getSkillTest >>= \case
        Nothing -> pure mempty
        Just st -> maybeModified_ a (SkillTestTarget st.id) do
          source <- MaybeT getSkillTestSource
          investigator <- MaybeT getSkillTestInvestigator
          guard $ isSource a source
          liftGuardM $ selectAny $ #exhausted <> withTrait Witch <> enemyAtLocationWith investigator
          pure [SkillTestAutomaticallySucceeds]
    pure $ threatArea <> skillTest

instance HasAbilities Bedeviled where
  getAbilities (Bedeviled a) = [skillTestAbility $ restrictedAbility a 1 OnSameLocation actionAbility]

instance RunMessage Bedeviled where
  runMessage msg t@(Bedeviled attrs) = runQueueT case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid attrs iid #willpower (Fixed 3)
      pure t
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> Bedeviled <$> liftRunMessage msg attrs
