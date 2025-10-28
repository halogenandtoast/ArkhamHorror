module Arkham.Skill.Cards.SelfSacrifice (selfSacrifice) where

import Arkham.Capability
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest
import Arkham.Message.Lifted.Choose
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype SelfSacrifice = SelfSacrifice SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

selfSacrifice :: SkillCard SelfSacrifice
selfSacrifice = skill SelfSacrifice Cards.selfSacrifice

instance HasModifiersFor SelfSacrifice where
  getModifiersFor (SelfSacrifice attrs) = do
    modified_ attrs attrs.controller [ResolvesFailedEffects]

instance RunMessage SelfSacrifice where
  runMessage msg s@(SelfSacrifice attrs) = runQueueT $ case msg of
    FailedSkillTest _ _ _ (isTarget attrs -> True) _ _ -> do
      performing <- fromJustNote "missing investigator" <$> getSkillTestInvestigator
      let you = attrs.controller
      chooseOrRunOneM you do
        whenM (can.draw.cards you) do
          targeting you $ drawCards you attrs 2
        whenM (can.draw.cards performing) do
          targeting performing $ drawCards performing attrs 2
      pure s
    _ -> SelfSacrifice <$> liftRunMessage msg attrs
