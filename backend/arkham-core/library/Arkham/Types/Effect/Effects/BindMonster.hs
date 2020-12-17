{-# LANGUAGE UndecidableInstances #-}

module Arkham.Types.Effect.Effects.BindMonster
  ( bindMonster
  , BindMonster(..)
  )
where

import Arkham.Import

import qualified Arkham.Types.Action as Action
import Arkham.Types.Effect.Attrs
import Arkham.Types.Trait

newtype BindMonster = BindMonster Attrs
  deriving newtype (Show, ToJSON, FromJSON)

bindMonster :: EffectArgs -> BindMonster
bindMonster = BindMonster . uncurry4 (baseAttrs "02031")

instance HasModifiersFor env BindMonster where
  getModifiersFor = noModifiersFor

instance (HasQueue env, HasSet Trait env EnemyId) => RunMessage env BindMonster where
  runMessage msg e@(BindMonster attrs@Attrs {..}) = case msg of
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
    SkillTestEnds -> e <$ unshiftMessage (DisableEffect effectId)
    _ -> BindMonster <$> runMessage msg attrs
