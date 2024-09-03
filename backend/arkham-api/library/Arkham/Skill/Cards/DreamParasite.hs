module Arkham.Skill.Cards.DreamParasite
  ( dreamParasite
  , DreamParasite(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype DreamParasite = DreamParasite SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dreamParasite :: SkillCard DreamParasite
dreamParasite = skill DreamParasite Cards.dreamParasite

instance RunMessage DreamParasite where
  runMessage msg (DreamParasite attrs) = DreamParasite <$> runMessage msg attrs
