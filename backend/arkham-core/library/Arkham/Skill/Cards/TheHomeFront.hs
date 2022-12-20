module Arkham.Skill.Cards.TheHomeFront
  ( theHomeFront
  , TheHomeFront(..)
  ) where

import Arkham.Prelude

import Arkham.Skill.Cards qualified as Cards
import Arkham.Action
import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Game.Helpers
import Arkham.Investigator.Types (Field(..))
import Arkham.Message hiding (InvestigatorDamage)
import Arkham.Projection
import Arkham.Skill.Runner
import Arkham.SkillTest
import Arkham.Target

newtype TheHomeFront = TheHomeFront SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHomeFront :: SkillCard TheHomeFront
theHomeFront = skill TheHomeFront Cards.theHomeFront

instance RunMessage TheHomeFront where
  runMessage msg s@(TheHomeFront attrs@SkillAttrs {..}) = case msg of
    PassedSkillTest _ (Just Fight) _ target _ _ | isTarget attrs target -> do
      mSkillTestTarget <- getSkillTestTarget
      damageCount <- field InvestigatorDamage skillOwner
      s <$ case mSkillTestTarget of
        Just (EnemyTarget eid) -> do
          canDamage <- sourceCanDamageEnemy eid (toSource attrs)
          when
            (canDamage && damageCount > 0)
            do
              pushAll
                [ HealDamage (InvestigatorTarget skillOwner) (toSource attrs) 1
                , EnemyDamage eid $ nonAttack attrs 1
                ]
        _ -> pure ()
    _ -> TheHomeFront <$> runMessage msg attrs
