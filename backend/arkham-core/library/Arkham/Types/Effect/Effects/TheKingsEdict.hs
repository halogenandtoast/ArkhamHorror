module Arkham.Types.Effect.Effects.TheKingsEdict
  ( TheKingsEdict(..)
  , theKingsEdict
  ) where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Effect.Attrs
import Arkham.Types.Game.Helpers
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.Target

newtype TheKingsEdict = TheKingsEdict EffectAttrs
  deriving anyclass HasAbilities
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theKingsEdict :: EffectArgs -> TheKingsEdict
theKingsEdict = TheKingsEdict . uncurry4 (baseAttrs "03100")

instance
  ( HasCount DoomCount env EnemyId
  , HasCount ClueCount env EnemyId
  )
  => HasModifiersFor env TheKingsEdict where
  getModifiersFor _ (EnemyTarget eid) (TheKingsEdict a) = do
    clueCount <- unClueCount <$> getCount eid
    doomCount <- unDoomCount <$> getCount eid
    pure $ toModifiers
      a
      [ EnemyFight (clueCount + doomCount) | clueCount + doomCount > 0 ]
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage env TheKingsEdict where
  runMessage msg e@(TheKingsEdict attrs) = case msg of
    EndRound -> e <$ push (DisableEffect $ toId e)
    _ -> TheKingsEdict <$> runMessage msg attrs
