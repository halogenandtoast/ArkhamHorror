module Arkham.Skill.Cards.NauticalProwess (nauticalProwess) where

import Arkham.Capability
import Arkham.Card
import Arkham.Matcher hiding (RevealChaosToken)
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype NauticalProwess = NauticalProwess SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nauticalProwess :: SkillCard NauticalProwess
nauticalProwess = skill NauticalProwess Cards.nauticalProwess

instance RunMessage NauticalProwess where
  runMessage msg s@(NauticalProwess attrs) = runQueueT $ case msg of
    RevealChaosToken _ _ token -> do
      whenM (token <=~> WithNegativeModifier) do
        onlyOnceDuringSkillTest attrs \sid -> do
          chooseOrRunOneM attrs.owner do
            labeled "Nautical Prowess gains {wild}{wild}"
              $ skillTestModifier sid attrs (toCardId attrs)
              $ AddSkillIcons [#wild, #wild]

            whenM (can.draw.cards attrs.owner) do
              labeled "Draw 1 card" $ drawCards attrs.owner attrs 1
      pure s
    _ -> NauticalProwess <$> liftRunMessage msg attrs
