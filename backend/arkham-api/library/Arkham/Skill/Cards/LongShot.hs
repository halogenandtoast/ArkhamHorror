module Arkham.Skill.Cards.LongShot (longShot) where

import Arkham.Action
import {-# SOURCE #-} Arkham.GameEnv (getSkillTest)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.SkillTest.Target
import Arkham.Matcher
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.SkillTestResult

newtype LongShot = LongShot SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

longShot :: SkillCard LongShot
longShot = skill LongShot Cards.longShot

instance HasModifiersFor LongShot where
  getModifiersFor (LongShot attrs) =
    modifySelf
      attrs.cardId
      [ CanCommitToSkillTestPerformedByAnInvestigatorAt
          (connectedFrom $ locationWithInvestigator attrs.owner)
      ]

instance RunMessage LongShot where
  runMessage msg s@(LongShot attrs) = runQueueT $ case msg of
    CheckSkillTestResultOptions skillTestId exclusions -> do
      mst <- getSkillTest
      for_ mst \st -> do
        when (st.id == skillTestId && isTarget attrs st.target) do
          case st.result of
            SucceededBy {} -> do
              when (st.action == Just Fight || st.action == Just Evade) do
                whenJustM getSkillTestTarget \case
                  EnemyTarget eid ->
                    provideSkillTestResultOption attrs exclusions "Long Shot" do
                      nonAttackEnemyDamage (Just attrs.owner) attrs 1 eid
                  _ -> error "invalid target"
            _ -> pure ()
      pure s
    _ -> LongShot <$> liftRunMessage msg attrs
