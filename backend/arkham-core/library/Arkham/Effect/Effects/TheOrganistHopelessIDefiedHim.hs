module Arkham.Effect.Effects.TheOrganistHopelessIDefiedHim
  ( TheOrganistHopelessIDefiedHim(..)
  , theOrganistHopelessIDefiedHim
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Game.Helpers
import Arkham.Message

newtype TheOrganistHopelessIDefiedHim = TheOrganistHopelessIDefiedHim EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theOrganistHopelessIDefiedHim :: EffectArgs -> TheOrganistHopelessIDefiedHim
theOrganistHopelessIDefiedHim =
  TheOrganistHopelessIDefiedHim . uncurry4 (baseAttrs "03221a")

instance HasModifiersFor TheOrganistHopelessIDefiedHim where
  getModifiersFor target (TheOrganistHopelessIDefiedHim attrs)
    | isTarget attrs target = pure $ toModifiers attrs [CannotAttack]
  getModifiersFor _ _ = pure []

instance RunMessage TheOrganistHopelessIDefiedHim where
  runMessage msg e@(TheOrganistHopelessIDefiedHim attrs) = case msg of
    EndRound -> e <$ push (DisableEffect $ toId attrs)
    _ -> TheOrganistHopelessIDefiedHim <$> runMessage msg attrs
