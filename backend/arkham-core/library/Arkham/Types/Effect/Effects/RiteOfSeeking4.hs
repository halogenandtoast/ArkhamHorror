module Arkham.Types.Effect.Effects.RiteOfSeeking4
  ( riteOfSeeking4
  , RiteOfSeeking4(..)
  ) where

import Arkham.Prelude

import Arkham.Types.Action qualified as Action
import Arkham.Types.Classes
import Arkham.Types.Effect.Attrs
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Token

newtype RiteOfSeeking4 = RiteOfSeeking4 EffectAttrs
  deriving anyclass HasAbilities
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riteOfSeeking4 :: EffectArgs -> RiteOfSeeking4
riteOfSeeking4 = RiteOfSeeking4 . uncurry4 (baseAttrs "02233")

instance HasModifiersFor env RiteOfSeeking4

instance (HasQueue env) => RunMessage env RiteOfSeeking4 where
  runMessage msg e@(RiteOfSeeking4 attrs@EffectAttrs {..}) = case msg of
    RevealToken _ iid token -> case effectTarget of
      InvestigationTarget iid' _ | iid == iid' -> e <$ when
        (tokenFace token `elem` [Skull, Cultist, Tablet, ElderThing, AutoFail])
        (push $ CreateEffect
          "02028"
          Nothing
          (toSource attrs)
          (InvestigatorTarget iid)
        )
      _ -> pure e
    SkillTestEnds _ -> e <$ case effectTarget of
      InvestigatorTarget iid -> pushAll [DisableEffect effectId, EndTurn iid]
      _ -> push (DisableEffect effectId)
    Successful (Action.Investigate, _) iid source _ _
      | effectSource == source -> case effectTarget of
        InvestigationTarget _ lid' ->
          e <$ push
            (InvestigatorDiscoverClues iid lid' 2 (Just Action.Investigate))
        _ -> pure e
    _ -> RiteOfSeeking4 <$> runMessage msg attrs
