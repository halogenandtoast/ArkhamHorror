module Arkham.Skill.Cards.ThreeAces1 (threeAces1) where

import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
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
          -- Passing resolves the skill test inline, discarding every committed
          -- copy. This copy's own commit is still queued behind us, and its
          -- ObtainCard would then sweep it back out of the discard, leaving it
          -- in no zone at all. Wait until the commit has fully resolved.
          inserted <- insertAfterMatchingMaybe [DoStep 1 msg] \case
            Do (CommitCard _ card) -> card.id == attrs.cardId
            _ -> False
          unless inserted $ doStep 1 msg
      ThreeAces1 <$> liftRunMessage msg attrs
    DoStep 1 (InvestigatorCommittedSkill iid sid) | sid == attrs.id -> do
      passSkillTest
      drawCards iid attrs 3
      gainResources iid attrs 3
      pure s
    _ -> ThreeAces1 <$> liftRunMessage msg attrs
