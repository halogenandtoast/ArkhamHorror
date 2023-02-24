module Arkham.Skill.Cards.HatchetMan
  ( hatchetMan
  , hatchetManEffect
  , HatchetMan(..)
  ) where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest
import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype HatchetMan = HatchetMan SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hatchetMan :: SkillCard HatchetMan
hatchetMan = skill HatchetMan Cards.hatchetMan

instance RunMessage HatchetMan where
  runMessage msg s@(HatchetMan attrs) = case msg of
    PassedSkillTest _ (Just Action.Evade) _ (isTarget attrs -> True) _ _ ->
      do
        target <- getSkillTestTarget
        case target of
          Just (EnemyTarget eid) -> push $ createCardEffect
            Cards.hatchetMan
            Nothing
            (toSource attrs)
            (EnemyTarget eid)
          _ -> pure ()
        pure s
    _ -> HatchetMan <$> runMessage msg attrs

newtype HatchetManEffect = HatchetManEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hatchetManEffect :: EffectArgs -> HatchetManEffect
hatchetManEffect = cardEffect HatchetManEffect Cards.hatchetMan

instance HasModifiersFor HatchetManEffect where
  getModifiersFor target (HatchetManEffect a) | effectTarget a == target =
    pure $ toModifiers a [DamageTaken 1]
  getModifiersFor _ _ = pure []

instance RunMessage HatchetManEffect where
  runMessage msg e@(HatchetManEffect attrs@EffectAttrs {..}) = case msg of
    EndTurn _ -> do
      push $ DisableEffect effectId
      pure e
    _ -> HatchetManEffect <$> runMessage msg attrs
