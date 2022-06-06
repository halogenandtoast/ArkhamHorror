module Arkham.Effect.Effects.FightOrFlight
  ( FightOrFlight(..)
  , fightOrFlight
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Attrs
import Arkham.Game.Helpers
import Arkham.Id
import Arkham.Message
import Arkham.Modifier
import Arkham.Query
import Arkham.SkillType
import Arkham.Target

newtype FightOrFlight = FightOrFlight EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fightOrFlight :: EffectArgs -> FightOrFlight
fightOrFlight = FightOrFlight . uncurry4 (baseAttrs "03155")

instance HasCount HorrorCount env InvestigatorId => HasModifiersFor FightOrFlight where
  getModifiersFor _ target@(InvestigatorTarget iid) (FightOrFlight attrs)
    | effectTarget attrs == target = do
      horror <- unHorrorCount <$> getCount iid
      pure $ toModifiers
        attrs
        [SkillModifier SkillCombat horror, SkillModifier SkillAgility horror]
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage FightOrFlight where
  runMessage msg e@(FightOrFlight attrs) = case msg of
    EndRound -> e <$ push (DisableEffect $ toId attrs)
    _ -> FightOrFlight <$> runMessage msg attrs
