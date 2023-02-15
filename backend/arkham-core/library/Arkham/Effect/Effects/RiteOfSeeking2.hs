module Arkham.Effect.Effects.RiteOfSeeking2
  ( riteOfSeeking2
  , RiteOfSeeking2(..)
  ) where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Message
import Arkham.Target
import Arkham.Token

newtype RiteOfSeeking2 = RiteOfSeeking2 EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riteOfSeeking2 :: EffectArgs -> RiteOfSeeking2
riteOfSeeking2 = RiteOfSeeking2 . uncurry4 (baseAttrs "51007")

instance RunMessage RiteOfSeeking2 where
  runMessage msg e@(RiteOfSeeking2 attrs@EffectAttrs {..}) = case msg of
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
    SkillTestEnds _ _ -> e <$ case effectTarget of
      InvestigatorTarget iid -> pushAll [DisableEffect effectId, EndTurn iid]
      _ -> push (DisableEffect effectId)
    Successful (Action.Investigate, _) iid source _ _
      | effectSource == source -> case effectTarget of
        InvestigationTarget _ lid' ->
          e <$ push
            (InvestigatorDiscoverClues iid lid' 1 (Just Action.Investigate))
        _ -> pure e
    _ -> RiteOfSeeking2 <$> runMessage msg attrs
