module Arkham.Effect.Effects.RiteOfSeeking2 (
  riteOfSeeking2,
  RiteOfSeeking2 (..),
) where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Message
import Arkham.ChaosToken
import Arkham.Window qualified as Window

newtype RiteOfSeeking2 = RiteOfSeeking2 EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riteOfSeeking2 :: EffectArgs -> RiteOfSeeking2
riteOfSeeking2 = RiteOfSeeking2 . uncurry4 (baseAttrs "51007")

instance RunMessage RiteOfSeeking2 where
  runMessage msg e@(RiteOfSeeking2 attrs@EffectAttrs {..}) = case msg of
    RevealChaosToken _ iid token -> case effectTarget of
      InvestigationTarget iid' _ | iid == iid' -> do
        when
          (chaosTokenFace token `elem` [Skull, Cultist, Tablet, ElderThing, AutoFail])
          ( pushAll
              [ If
                  (Window.RevealChaosTokenEffect iid token effectId)
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
    _ -> RiteOfSeeking2 <$> runMessage msg attrs
