module Arkham.Effect.Effects.BindMonster2
  ( bindMonster2
  , BindMonster2(..)
  ) where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Enemy.Types
import Arkham.Projection
import Arkham.Message
import Arkham.Source
import Arkham.Target
import Arkham.Trait

newtype BindMonster2 = BindMonster2 EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bindMonster2 :: EffectArgs -> BindMonster2
bindMonster2 = BindMonster2 . uncurry4 (baseAttrs "02031")

instance RunMessage BindMonster2 where
  runMessage msg e@(BindMonster2 attrs@EffectAttrs {..}) = case msg of
    PassedSkillTest _ (Just Action.Evade) _ (SkillTestInitiatorTarget (EnemyTarget eid)) _ _
      | SkillTestTarget == effectTarget
      -> case effectSource of
        (EventSource evid) -> do
          nonElite <- notMember Elite <$> field EnemyTraits eid
          e <$ when
            nonElite
            (pushAll
              [AttachEvent evid (EnemyTarget eid), DisableEffect effectId]
            )
        _ -> pure e
    SkillTestEnds _ -> e <$ push (DisableEffect effectId)
    _ -> BindMonster2 <$> runMessage msg attrs
