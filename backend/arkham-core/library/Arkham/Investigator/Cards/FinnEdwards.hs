module Arkham.Investigator.Cards.FinnEdwards
  ( finnEdwards
  , FinnEdwards(..)
  )
where

import Arkham.Prelude

import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Action.Additional
import Arkham.Action qualified as Action

newtype FinnEdwards = FinnEdwards InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

finnEdwards :: InvestigatorCard FinnEdwards
finnEdwards = investigator
  FinnEdwards
  Cards.finnEdwards
  Stats
    { health = 7
    , sanity = 7
    , willpower = 1
    , intellect = 4
    , combat = 3
    , agility = 4
    }

instance HasAbilities FinnEdwards where
  getAbilities (FinnEdwards _) = []

instance HasTokenValue FinnEdwards where
  getTokenValue iid ElderSign (FinnEdwards attrs) | iid == toId attrs = do
    pure $ TokenValue ElderSign NoModifier
  getTokenValue _ token _ = pure $ TokenValue token mempty

instance RunMessage FinnEdwards where
  runMessage msg (FinnEdwards attrs) = case msg of
    Setup -> FinnEdwards <$> runMessage
      msg
      (attrs & additionalActionsL %~ (ActionRestrictedAdditionalAction Action.Evade :))
    BeginRound -> FinnEdwards <$> runMessage
      msg
      (attrs & additionalActionsL %~ (ActionRestrictedAdditionalAction Action.Evade :))
    _ -> FinnEdwards <$> runMessage msg attrs
