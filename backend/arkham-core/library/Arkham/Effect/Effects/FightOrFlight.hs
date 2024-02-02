module Arkham.Effect.Effects.FightOrFlight (
  FightOrFlight (..),
  fightOrFlight,
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Game.Helpers
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.SkillType

newtype FightOrFlight = FightOrFlight EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

fightOrFlight :: EffectArgs -> FightOrFlight
fightOrFlight = FightOrFlight . uncurry4 (baseAttrs "03155")

instance HasModifiersFor FightOrFlight where
  getModifiersFor target@(InvestigatorTarget iid) (FightOrFlight attrs)
    | effectTarget attrs == target = do
        horror <- field InvestigatorHorror iid
        pure
          $ toModifiers
            attrs
            [SkillModifier SkillCombat horror, SkillModifier SkillAgility horror]
  getModifiersFor _ _ = pure []

instance RunMessage FightOrFlight where
  runMessage msg e@(FightOrFlight attrs) = case msg of
    EndRound -> e <$ push (DisableEffect $ toId attrs)
    _ -> FightOrFlight <$> runMessage msg attrs
