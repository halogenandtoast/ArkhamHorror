module Arkham.Skill.Cards.ThreeAces1 (threeAces1, ThreeAces1 (..)) where

import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.Strategy

newtype ThreeAces1 = ThreeAces1 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

threeAces1 :: SkillCard ThreeAces1
threeAces1 = skill ThreeAces1 Cards.threeAces1

instance RunMessage ThreeAces1 where
  runMessage msg (ThreeAces1 attrs) = runQueueT $ case msg of
    InvestigatorCommittedSkill iid sid | sid == attrs.id -> do
      withSkillTest \stId -> do
        n <- select $ skillIs Cards.threeAces1 <> skillControlledBy iid
        mods <- getModifiers (SkillTestTarget stId)
        when (length n >= 3 && MetaModifier "ThreeAces1" `notElem` mods) $ do
          for_ n \copy -> skillTestModifier stId (toSource attrs) copy (SetAfterPlay RemoveThisFromGame)
          skillTestModifier stId attrs sid (MetaModifier "ThreeAces1")
          passSkillTest
          drawCardsIfCan iid attrs 3
          gainResourcesIfCan iid attrs 3
      ThreeAces1 <$> liftRunMessage msg attrs
    _ -> ThreeAces1 <$> liftRunMessage msg attrs
