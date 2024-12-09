module Arkham.Event.Events.FightOrFlight (fightOrFlight, fightOrFlightEffect, FightOrFlight (..)) where

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Prelude
import Arkham.Projection

newtype FightOrFlight = FightOrFlight EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fightOrFlight :: EventCard FightOrFlight
fightOrFlight = event FightOrFlight Cards.fightOrFlight

instance RunMessage FightOrFlight where
  runMessage msg e@(FightOrFlight attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      push =<< createCardEffect Cards.fightOrFlight Nothing attrs iid
      pure e
    _ -> FightOrFlight <$> runMessage msg attrs

newtype FightOrFlightEffect = FightOrFlightEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fightOrFlightEffect :: EffectArgs -> FightOrFlightEffect
fightOrFlightEffect = cardEffect FightOrFlightEffect Cards.fightOrFlight

instance HasModifiersFor FightOrFlightEffect where
  getModifiersFor (FightOrFlightEffect attrs) = case attrs.target of
    InvestigatorTarget iid -> do
      horror <- field InvestigatorHorror iid
      modified_ attrs iid [SkillModifier #combat horror, SkillModifier #agility horror]
    _ -> pure mempty

instance RunMessage FightOrFlightEffect where
  runMessage msg e@(FightOrFlightEffect attrs) = case msg of
    EndRound -> do
      push $ disable attrs
      pure e
    _ -> FightOrFlightEffect <$> runMessage msg attrs
