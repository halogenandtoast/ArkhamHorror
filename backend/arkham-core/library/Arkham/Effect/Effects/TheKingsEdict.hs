module Arkham.Effect.Effects.TheKingsEdict
  ( TheKingsEdict(..)
  , theKingsEdict
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Enemy.Types (Field(EnemyClues, EnemyDoom))
import Arkham.Game.Helpers
import Arkham.Message
import Arkham.Projection
import Arkham.Target

newtype TheKingsEdict = TheKingsEdict EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theKingsEdict :: EffectArgs -> TheKingsEdict
theKingsEdict = TheKingsEdict . uncurry4 (baseAttrs "03100")

instance HasModifiersFor TheKingsEdict where
  getModifiersFor _ target@(EnemyTarget eid) (TheKingsEdict a)
    | target == effectTarget a = do
      clueCount <- field EnemyClues eid
      doomCount <- field EnemyDoom eid
      pure $ toModifiers
        a
        [ EnemyFight (clueCount + doomCount) | clueCount + doomCount > 0 ]
  getModifiersFor _ _ _ = pure []

instance RunMessage TheKingsEdict where
  runMessage msg e@(TheKingsEdict attrs) = case msg of
    EndRound -> e <$ push (DisableEffect $ toId e)
    _ -> TheKingsEdict <$> runMessage msg attrs
