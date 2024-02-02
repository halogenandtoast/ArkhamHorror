module Arkham.Skill.Cards.Eureka (
  eureka,
  Eureka (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype Eureka = Eureka SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

eureka :: SkillCard Eureka
eureka = skill Eureka Cards.eureka

instance RunMessage Eureka where
  runMessage msg s@(Eureka attrs) = case msg of
    PassedSkillTest iid _ _ (isTarget attrs -> True) _ _ -> do
      push $ search iid attrs iid [fromTopOfDeck 3] AnyCard (DrawFound iid 1)
      pure s
    _ -> Eureka <$> runMessage msg attrs
