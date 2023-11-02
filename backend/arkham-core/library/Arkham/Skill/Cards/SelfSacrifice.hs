module Arkham.Skill.Cards.SelfSacrifice (
  selfSacrifice,
  SelfSacrifice (..),
)
where

import Arkham.Prelude

import Arkham.Capability
import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype SelfSacrifice = SelfSacrifice SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

selfSacrifice :: SkillCard SelfSacrifice
selfSacrifice = skill SelfSacrifice Cards.selfSacrifice

instance HasModifiersFor SelfSacrifice where
  getModifiersFor (InvestigatorTarget iid) (SelfSacrifice attrs) = do
    pure $ toModifiers attrs [ResolvesFailedEffects | attrs.controller == iid]
  getModifiersFor _ _ = pure []

instance RunMessage SelfSacrifice where
  runMessage msg s@(SelfSacrifice attrs) = case msg of
    FailedSkillTest _ _ _ (isTarget attrs -> True) _ _ -> do
      performing <- fromJustNote "missing investigator" <$> getSkillTestInvestigator
      let you = attrs.controller

      youCanDraw <- can.draw.cards you
      performingCanDraw <- can.draw.cards performing

      when (youCanDraw || performingCanDraw) $ do
        player <- getPlayer you
        yourDraw <- drawCards you attrs 2
        performingDraw <- drawCards performing attrs 2
        push
          $ chooseOrRunOne player
          $ [targetLabel you [yourDraw] | youCanDraw]
          <> [targetLabel performing [performingDraw] | performingCanDraw]

      pure s
    _ -> SelfSacrifice <$> runMessage msg attrs
