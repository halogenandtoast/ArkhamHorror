module Arkham.Skill.Cards.Contemplative (contemplative) where

import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.SkillTest
import Arkham.Matcher
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.SkillTestResult

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
    CheckSkillTestResultOptions skillTestId exclusions -> do
      mst <- getSkillTest
      for_ mst \st -> do
        when (st.id == skillTestId && isTarget attrs st.target) do
          case st.result of
            SucceededBy {} -> do
              withSkillTestInvestigator \iid -> do
                provideSkillTestResultOption attrs exclusions "Contemplative" do
                  discoverAtYourLocation NotInvestigate iid (attrs.ability 1) 1
            _ -> pure ()
      pure s
    _ -> Contemplative <$> liftRunMessage msg attrs
