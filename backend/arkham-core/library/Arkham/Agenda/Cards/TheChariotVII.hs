module Arkham.Agenda.Cards.TheChariotVII (
  TheChariotVII (..),
  theChariotVII,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Scenarios.InTheClutchesOfChaos.Helpers
import Arkham.Timing qualified as Timing
import Arkham.Window (Window (windowBatchId))

newtype TheChariotVII = TheChariotVII AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theChariotVII :: AgendaCard TheChariotVII
theChariotVII = agenda (1, A) TheChariotVII Cards.theChariotVII (Static 7)

instance HasAbilities TheChariotVII where
  getAbilities (TheChariotVII a)
    | onSide A a =
        [ mkAbility a 1 $ ForcedAbility $ WouldPlaceDoomCounter Timing.When AnySource (TargetIs $ toTarget a)
        ]
  getAbilities _ = []

getBatchId :: [Window] -> BatchId
getBatchId ((windowBatchId -> Just batchId) : _) = batchId
getBatchId (_ : rest) = getBatchId rest
getBatchId [] = error "No batch id found"

instance RunMessage TheChariotVII where
  runMessage msg a@(TheChariotVII attrs) =
    case msg of
      UseCardAbility _ (isSource attrs -> True) 1 (getBatchId -> batchId) _ -> do
        playerCount <- getPlayerCount
        locations <- sampleLocations (playerCount + 1)
        pushAll $ IgnoreBatch batchId : map (PlaceBreach . toTarget) locations
        pure a
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
        pushAll [advanceAgendaDeck attrs]
        pure a
      _ -> TheChariotVII <$> runMessage msg attrs
