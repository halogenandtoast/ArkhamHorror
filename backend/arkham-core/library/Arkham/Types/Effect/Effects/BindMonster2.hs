module Arkham.Types.Effect.Effects.BindMonster2
  ( bindMonster2
  , BindMonster2(..)
  ) where

import Arkham.Prelude

import Arkham.Types.Action qualified as Action
import Arkham.Types.Classes
import Arkham.Types.Effect.Attrs
import Arkham.Types.EnemyId
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Trait

newtype BindMonster2 = BindMonster2 EffectAttrs
  deriving anyclass HasAbilities
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bindMonster2 :: EffectArgs -> BindMonster2
bindMonster2 = BindMonster2 . uncurry4 (baseAttrs "02031")

instance HasModifiersFor env BindMonster2

instance (HasQueue env, HasSet Trait env EnemyId) => RunMessage env BindMonster2 where
  runMessage msg e@(BindMonster2 attrs@EffectAttrs {..}) = case msg of
    PassedSkillTest _ (Just Action.Evade) _ (SkillTestInitiatorTarget (EnemyTarget eid)) _ _
      | SkillTestTarget == effectTarget
      -> case effectSource of
        (EventSource evid) -> do
          nonElite <- notMember Elite <$> getSet eid
          e <$ when
            nonElite
            (pushAll
              [AttachEvent evid (EnemyTarget eid), DisableEffect effectId]
            )
        _ -> pure e
    SkillTestEnds _ -> e <$ push (DisableEffect effectId)
    _ -> BindMonster2 <$> runMessage msg attrs
