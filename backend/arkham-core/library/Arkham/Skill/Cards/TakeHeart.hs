module Arkham.Skill.Cards.TakeHeart (
  takeHeart,
  TakeHeart (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype TakeHeart = TakeHeart SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

takeHeart :: SkillCard TakeHeart
takeHeart = skill TakeHeart Cards.takeHeart

instance RunMessage TakeHeart where
  runMessage msg s@(TakeHeart attrs) = case msg of
    FailedSkillTest iid _ _ (SkillTarget sid) _ _ | sid == toId attrs -> do
      drawing <- drawCards iid attrs 2
      pushAll [drawing, TakeResources iid 2 (toSource attrs) False]
      pure s
    _ -> TakeHeart <$> runMessage msg attrs
