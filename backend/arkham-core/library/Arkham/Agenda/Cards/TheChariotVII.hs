module Arkham.Agenda.Cards.TheChariotVII (
  TheChariotVII (..),
  theChariotVII,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.CampaignLogKey
import Arkham.Classes
import Arkham.GameValue
import Arkham.Id
import Arkham.Matcher
import Arkham.Scenarios.InTheClutchesOfChaos.Helpers
import Arkham.Timing qualified as Timing
import Arkham.Window (Window (windowType), getBatchId)
import Arkham.Window qualified as Window

newtype TheChariotVII = TheChariotVII AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

theChariotVII :: AgendaCard TheChariotVII
theChariotVII = agenda (1, A) TheChariotVII Cards.theChariotVII (Static 7)

instance HasAbilities TheChariotVII where
  getAbilities (TheChariotVII a)
    | onSide A a =
        [ forcedAbility a 1 $ WouldPlaceDoomCounter Timing.When AnySource (TargetIs $ toTarget a)
        , forcedAbility a 2
            $ WouldPlaceBreach Timing.When
            $ LocationTargetMatches (LocationWithBreaches $ EqualTo $ Static 3)
        ]
  getAbilities _ = []

getWindowLocation :: [Window] -> LocationId
getWindowLocation ((windowType -> Window.WouldPlaceBreach (LocationTarget lid)) : _) = lid
getWindowLocation (_ : rest) = getWindowLocation rest
getWindowLocation [] = error "No location id found"

instance RunMessage TheChariotVII where
  runMessage msg a@(TheChariotVII attrs) =
    case msg of
      UseCardAbility _ (isSource attrs -> True) 1 (getBatchId -> batchId) _ -> do
        playerCount <- getPlayerCount
        locations <- sampleLocations (playerCount + 1)
        lead <- getLeadPlayer
        pushAll
          [ IgnoreBatch batchId
          , chooseOneAtATime lead
              $ [targetLabel location [PlaceBreaches (toTarget location) 1] | location <- locations]
          ]
        pure a
      UseCardAbility _ (isSource attrs -> True) 2 (getBatchId &&& getWindowLocation -> (batchId, lid)) _ -> do
        pushAll [IgnoreBatch batchId, Incursion lid]
        pure a
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
        anetteMasonIsPossessedByEvil <- getHasRecord AnetteMasonIsPossessedByEvil
        push $ if anetteMasonIsPossessedByEvil then R3 else R4
        pure a
      _ -> TheChariotVII <$> runMessage msg attrs
