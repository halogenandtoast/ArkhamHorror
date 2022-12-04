module Arkham.Skill.Cards.AllIn5
  ( allIn5
  , AllIn5(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Draw.Types
import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype AllIn5 = AllIn5 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

allIn5 :: SkillCard AllIn5
allIn5 = skill AllIn5 Cards.allIn5

instance RunMessage AllIn5 where
  runMessage msg s@(AllIn5 attrs) = case msg of
    PassedSkillTest iid _ _ (isTarget attrs -> True) _ _ -> do
      drawing <- withCardDrawRule ShuffleBackInEachWeakness
        <$> newCardDraw iid attrs 5
      push $ DrawCards drawing
      pure s
    _ -> AllIn5 <$> runMessage msg attrs
