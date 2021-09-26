module Arkham.Types.Effect.Effects.FightOrFlight
  ( FightOrFlight(..)
  , fightOrFlight
  ) where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Effect.Attrs
import Arkham.Types.Game.Helpers
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype FightOrFlight = FightOrFlight EffectAttrs
  deriving anyclass HasAbilities
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fightOrFlight :: EffectArgs -> FightOrFlight
fightOrFlight = FightOrFlight . uncurry4 (baseAttrs "03155")

instance HasCount HorrorCount env InvestigatorId => HasModifiersFor env FightOrFlight where
  getModifiersFor _ target@(InvestigatorTarget iid) (FightOrFlight attrs)
    | effectTarget attrs == target = do
      horror <- unHorrorCount <$> getCount iid
      pure $ toModifiers
        attrs
        [SkillModifier SkillCombat horror, SkillModifier SkillAgility horror]
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage env FightOrFlight where
  runMessage msg e@(FightOrFlight attrs) = case msg of
    EndRound -> e <$ push (DisableEffect $ toId attrs)
    _ -> FightOrFlight <$> runMessage msg attrs
