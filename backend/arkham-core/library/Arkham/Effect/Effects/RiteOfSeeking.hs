module Arkham.Effect.Effects.RiteOfSeeking (
  riteOfSeeking,
  RiteOfSeeking (..),
) where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Message
import Arkham.Token
import Arkham.Window qualified as Window

newtype RiteOfSeeking = RiteOfSeeking EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riteOfSeeking :: EffectArgs -> RiteOfSeeking
riteOfSeeking = RiteOfSeeking . uncurry4 (baseAttrs "02028")

instance RunMessage RiteOfSeeking where
  runMessage msg e@(RiteOfSeeking attrs@EffectAttrs {..}) = case msg of
    RevealToken _ iid token -> case effectTarget of
      InvestigationTarget iid' _ | iid == iid' -> do
        when
          (tokenFace token `elem` [Skull, Cultist, Tablet, ElderThing, AutoFail])
          ( pushAll
              [ If
                  (Window.RevealTokenEffect iid token effectId)
                  [SetActions iid effectSource 0, ChooseEndTurn iid]
              , DisableEffect effectId
              ]
          )
        pure e
      _ -> pure e
    SkillTestEnds _ _ -> do
      case effectTarget of
        InvestigatorTarget iid -> pushAll [DisableEffect effectId, EndTurn iid]
        _ -> push (DisableEffect effectId)
      pure e
    Successful (Action.Investigate, _) iid source _ _
      | effectSource == source -> case effectTarget of
          InvestigationTarget _ lid' -> do
            push
              (InvestigatorDiscoverClues iid lid' (toSource attrs) 1 (Just Action.Investigate))
            pure e
          _ -> pure e
    _ -> RiteOfSeeking <$> runMessage msg attrs
