module Arkham.Treachery.Cards.Wracked (wracked, Wracked (..)) where

import Arkham.Ability
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest.Lifted hiding (beginSkillTest)
import Arkham.History
import Arkham.Matcher
import Arkham.Placement
import Arkham.Source
import Arkham.Trait (Trait (Witch))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Wracked = Wracked TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wracked :: TreacheryCard Wracked
wracked = treachery Wracked Cards.wracked

instance HasModifiersFor Wracked where
  getModifiersFor (Wracked a) = do
    investigator <- case a.placement of
      InThreatArea iid -> maybeModified_ a iid do
        isSkillTestInvestigator iid
        liftGuardM $ null . historySkillTestsPerformed <$> getHistory RoundHistory iid
        pure [AnySkillValue (-1)]
      _ -> pure mempty

    skillTest <-
      getSkillTest >>= \case
        Nothing -> pure mempty
        Just st -> maybeModified_ a (SkillTestTarget st.id) do
          isSkillTestSource a
          guardInThreatArea st.investigator a
          liftGuardM $ selectAny $ #exhausted <> withTrait Witch <> enemyAtLocationWith st.investigator
          pure [SkillTestAutomaticallySucceeds]

    pure $ investigator <> skillTest

instance HasAbilities Wracked where
  getAbilities (Wracked a) = [skillTestAbility $ restrictedAbility a 1 OnSameLocation actionAbility]

instance RunMessage Wracked where
  runMessage msg t@(Wracked attrs) = runQueueT $ case msg of
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
    _ -> Wracked <$> liftRunMessage msg attrs
