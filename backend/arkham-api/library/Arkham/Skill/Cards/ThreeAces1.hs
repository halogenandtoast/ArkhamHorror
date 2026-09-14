module Arkham.Skill.Cards.ThreeAces1 (threeAces1) where

import Arkham.Helpers.SkillTest (getsSkillTest, withSkillTest)
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.SkillTest.Step
import Arkham.Strategy
import Arkham.Taboo

newtype ThreeAces1 = ThreeAces1 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

threeAces1 :: SkillCard ThreeAces1
threeAces1 = skill ThreeAces1 Cards.threeAces1

instance RunMessage ThreeAces1 where
  runMessage msg s@(ThreeAces1 attrs) = runQueueT $ case msg of
    InvestigatorCommittedSkill iid sid | sid == attrs.id -> do
      withSkillTest \stId -> do
        n <- select $ skillIs Cards.threeAces1 <> skillOwnedBy iid
        mods <- getModifiers (SkillTestTarget stId)
        when (length n >= 3 && MetaModifier "ThreeAces1" `notElem` mods) do
          when (tabooed TabooList19 attrs) do
            for_ n \copy -> skillTestModifier stId attrs copy (SetAfterPlay RemoveThisFromGame)
          skillTestModifier stId attrs stId (MetaModifier "ThreeAces1")
          -- Passing the test here ended it inside the commit window, so anything
          -- committed after us joined a test that no longer existed (#5540). Let
          -- TriggerSkillTest apply the automatic success at ST.5 instead.
          skillTestModifier stId attrs stId SkillTestAutomaticallySucceeds
          -- This copy owns the "Then ..." clause, so it fires max once per test.
          skillTestModifier stId attrs sid (MetaModifier "ThreeAces1.Resolves")
          -- Committing this late (e.g. Isabelle Barnes) is past TriggerSkillTest,
          -- which will never read the modifier, so pass the test directly.
          step <- getsSkillTest (.step)
          when (maybe False (>= RevealChaosTokenStep) step) passSkillTest
      ThreeAces1 <$> liftRunMessage msg attrs
    PassedSkillTest _ _ _ (isTarget attrs -> True) _ _ -> do
      mods <- getModifiers attrs
      when (MetaModifier "ThreeAces1.Resolves" `elem` mods) do
        drawCards attrs.owner attrs 3
        gainResources attrs.owner attrs 3
      pure s
    _ -> ThreeAces1 <$> liftRunMessage msg attrs
