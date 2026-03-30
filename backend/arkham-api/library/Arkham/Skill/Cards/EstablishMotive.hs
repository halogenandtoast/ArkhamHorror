module Arkham.Skill.Cards.EstablishMotive (establishMotive) where

import Arkham.Matcher
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.Strategy
import Arkham.Trait (Trait (Insight))

newtype EstablishMotive = EstablishMotive SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

establishMotive :: SkillCard EstablishMotive
establishMotive = skill EstablishMotive Cards.establishMotive

instance RunMessage EstablishMotive where
  runMessage msg s@(EstablishMotive attrs) = runQueueT $ case msg of
    PassedSkillTest iid _ _ (isTarget attrs -> True) _ _ -> do
      search iid attrs iid [fromTopOfDeck 6] (basic $ #event <> withTrait Insight) (DrawFound iid 1)
      pure s
    _ -> EstablishMotive <$> liftRunMessage msg attrs
