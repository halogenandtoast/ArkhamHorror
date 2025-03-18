module Arkham.Skill.Cards.Eureka (eureka) where

import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype Eureka = Eureka SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eureka :: SkillCard Eureka
eureka = skill Eureka Cards.eureka

instance RunMessage Eureka where
  runMessage msg s@(Eureka attrs) = runQueueT $ case msg of
    PassedSkillTest iid _ _ (isTarget attrs -> True) _ _ -> do
      skillTestResultOption "Eureka" do
        search iid attrs iid [fromTopOfDeck 3] #any (DrawFound iid 1)
      pure s
    _ -> Eureka <$> liftRunMessage msg attrs
