module Arkham.Types.Skill.Cards.TakeHeart
  ( takeHeart
  , TakeHeart(..)
  ) where

import Arkham.Prelude

import Arkham.Skill.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Skill.Attrs
import Arkham.Types.Skill.Runner
import Arkham.Types.Target

newtype TakeHeart = TakeHeart SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

takeHeart :: SkillCard TakeHeart
takeHeart = skill TakeHeart Cards.takeHeart

instance SkillRunner env => RunMessage env TakeHeart where
  runMessage msg s@(TakeHeart attrs) = case msg of
    FailedSkillTest iid _ _ (SkillTarget sid) _ _ | sid == toId attrs ->
      s <$ pushAll [DrawCards iid 2 False, TakeResources iid 2 False]
    _ -> TakeHeart <$> runMessage msg attrs
