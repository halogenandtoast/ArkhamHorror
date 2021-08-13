module Arkham.Types.Skill.Cards.TheHomeFront
  ( theHomeFront
  , TheHomeFront(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Skill.Cards as Cards
import Arkham.Types.Action
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Skill.Attrs
import Arkham.Types.Skill.Runner
import Arkham.Types.SkillTest
import Arkham.Types.Target

newtype TheHomeFront = TheHomeFront SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor env, HasActions)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHomeFront :: SkillCard TheHomeFront
theHomeFront = skill TheHomeFront Cards.theHomeFront

instance
  ( HasSkillTest env
  , HasCount DamageCount env InvestigatorId
  , SkillRunner env
  )
  => RunMessage env TheHomeFront where
  runMessage msg s@(TheHomeFront attrs@SkillAttrs {..}) = case msg of
    PassedSkillTest _ (Just Fight) _ target _ _ | isTarget attrs target -> do
      mSkillTestTarget <- getSkillTestTarget
      damageCount <- unDamageCount <$> getCount skillOwner
      s <$ case mSkillTestTarget of
        Just (EnemyTarget eid) -> when
          (damageCount > 0)
          (pushAll
            [ HealDamage (InvestigatorTarget skillOwner) 1
            , EnemyDamage eid skillOwner (toSource attrs) 1
            ]
          )
        _ -> pure ()
    _ -> TheHomeFront <$> runMessage msg attrs
