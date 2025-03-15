module Arkham.Skill.Cards.HatchetMan (hatchetMan, hatchetManEffect) where

import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Effect.Import
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest
import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype HatchetMan = HatchetMan SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hatchetMan :: SkillCard HatchetMan
hatchetMan = skill HatchetMan Cards.hatchetMan

instance RunMessage HatchetMan where
  runMessage msg s@(HatchetMan attrs) = runQueueT $ case msg of
    PassedSkillTest _ (Just Action.Evade) _ (isTarget attrs -> True) _ _ -> do
      getSkillTestTargetedEnemy >>= traverse_ (createCardEffect Cards.hatchetMan Nothing attrs)
      pure s
    _ -> HatchetMan <$> liftRunMessage msg attrs

newtype HatchetManEffect = HatchetManEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hatchetManEffect :: EffectArgs -> HatchetManEffect
hatchetManEffect = cardEffect HatchetManEffect Cards.hatchetMan

instance HasModifiersFor HatchetManEffect where
  getModifiersFor (HatchetManEffect a) = modified_ a a.target [DamageTaken 1]

instance RunMessage HatchetManEffect where
  runMessage msg e@(HatchetManEffect attrs) = runQueueT $ case msg of
    EndTurn _ -> disableReturn e
    EnemyDamaged eid _ | isTarget eid attrs.target -> disableReturn e
    _ -> HatchetManEffect <$> liftRunMessage msg attrs
