{-# LANGUAGE UndecidableInstances #-}

module Arkham.Types.Effect.Effects.BindMonster2
  ( bindMonster2
  , BindMonster2(..)
  )
where

import Arkham.Import

import qualified Arkham.Types.Action as Action
import Arkham.Types.Effect.Attrs
import Arkham.Types.Trait

newtype BindMonster2 = BindMonster2 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

bindMonster2 :: EffectArgs -> BindMonster2
bindMonster2 = BindMonster2 . uncurry4 (baseAttrs "02031")

instance HasModifiersFor env BindMonster2 where
  getModifiersFor = noModifiersFor

instance (HasQueue env, HasSet Trait env EnemyId) => RunMessage env BindMonster2 where
  runMessage msg e@(BindMonster2 attrs@Attrs {..}) = case msg of
    PassedSkillTest _ (Just Action.Evade) _ (SkillTestInitiatorTarget (EnemyTarget eid)) _
      | SkillTestTarget == effectTarget
      -> case effectSource of
        (EventSource evid) -> do
          nonElite <- notMember Elite <$> getSet eid
          e <$ when
            nonElite
            (unshiftMessages
              [AttachEvent evid (EnemyTarget eid), DisableEffect effectId]
            )
        _ -> pure e
    SkillTestEnds _ -> e <$ unshiftMessage (DisableEffect effectId)
    _ -> BindMonster2 <$> runMessage msg attrs
