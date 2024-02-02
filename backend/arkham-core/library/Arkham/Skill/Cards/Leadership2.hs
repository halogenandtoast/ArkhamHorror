module Arkham.Skill.Cards.Leadership2 (
  leadership2,
  Leadership2 (..),
) where

import Arkham.Prelude

import Arkham.Capability
import Arkham.Card
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.Helpers.SkillTest
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype Leadership2 = Leadership2 SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

leadership2 :: SkillCard Leadership2
leadership2 = skill Leadership2 Cards.leadership2

instance HasModifiersFor Leadership2 where
  getModifiersFor (CardIdTarget cid) (Leadership2 attrs) | toCardId attrs == cid = do
    mInvestigator <- getSkillTestInvestigator
    pure $ case mInvestigator of
      Just iid | skillOwner attrs /= iid -> do
        toModifiers attrs [AddSkillIcons [#willpower, #wild]]
      _ -> []
  getModifiersFor _ _ = pure []

instance RunMessage Leadership2 where
  runMessage msg s@(Leadership2 attrs) = case msg of
    PassedSkillTest iid _ _ (isTarget attrs -> True) _ _ -> do
      youCanGainResources <- can.gain.resources (skillOwner attrs)
      otherCanGainResources <- can.gain.resources iid

      pushAll
        $ [takeResources (skillOwner attrs) attrs 1 | youCanGainResources]
        <> [takeResources iid attrs 1 | otherCanGainResources, iid /= skillOwner attrs]

      pure s
    _ -> Leadership2 <$> runMessage msg attrs
