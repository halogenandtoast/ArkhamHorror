module Arkham.Skill.Cards.Contemplative (contemplative) where

import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.SkillTest
import Arkham.Matcher
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype Contemplative = Contemplative SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

contemplative :: SkillCard Contemplative
contemplative = skill Contemplative Cards.contemplative

instance HasModifiersFor Contemplative where
  getModifiersFor (Contemplative attrs) =
    modifySelf
      attrs.cardId
      [ CanCommitToSkillTestPerformedByAnInvestigatorAt
          (connectedFrom $ locationWithInvestigator attrs.owner)
      ]

instance RunMessage Contemplative where
  runMessage msg s@(Contemplative attrs) = runQueueT $ case msg of
    PassedSkillTest _iid _ _ (isTarget attrs -> True) _ _ -> do
      withSkillTestInvestigator \iid -> do
        additionalSkillTestOption "Contemplative" do
          discoverAtYourLocation NotInvestigate iid (attrs.ability 1) 1
      pure s
    _ -> Contemplative <$> liftRunMessage msg attrs
