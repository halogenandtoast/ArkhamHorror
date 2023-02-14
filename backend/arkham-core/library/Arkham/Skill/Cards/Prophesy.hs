module Arkham.Skill.Cards.Prophesy
  ( prophesy
  , Prophesy(..)
  )
where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner
import Arkham.SkillType
import Arkham.Target

newtype Prophesy = Prophesy SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

prophesy :: SkillCard Prophesy
prophesy =
  skill Prophesy Cards.prophesy

instance HasModifiersFor Prophesy where
  getModifiersFor (CardIdTarget cid) (Prophesy attrs) | toCardId attrs == cid =
    do
      doom <- getDoomCount
      pure $ toModifiers
        attrs
        [ AddSkillIcons $ if doom >= 6
            then
              [ WildIcon
              , WildIcon
              ]
            else [WildIcon]
        | doom >= 3
        ]
  getModifiersFor _ _ = pure []

instance RunMessage Prophesy where
  runMessage msg (Prophesy attrs) = Prophesy <$> runMessage msg attrs
