module Arkham.Agenda.Cards.TheChariotVII (TheChariotVII (..), theChariotVII) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.CampaignLogKey
import Arkham.Helpers.Log (getHasRecord)
import Arkham.Helpers.Query (getLead, getPlayerCount)
import Arkham.Id
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.InTheClutchesOfChaos.Helpers
import Arkham.Window (Window (windowType), getBatchId)
import Arkham.Window qualified as Window

newtype TheChariotVII = TheChariotVII AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theChariotVII :: AgendaCard TheChariotVII
theChariotVII = agenda (1, A) TheChariotVII Cards.theChariotVII (Static 7)

instance HasAbilities TheChariotVII where
  getAbilities (TheChariotVII a)
    | onSide A a =
        [ forcedAbility a 1 $ WouldPlaceDoomCounter #when AnySource (TargetIs $ toTarget a)
        , noLimit
            $ forcedAbility a 2
            $ WouldPlaceBreach #when
            $ LocationTargetMatches (LocationWithBreaches $ atLeast 3)
        ]
  getAbilities _ = []

getWindowLocation :: [Window] -> LocationId
getWindowLocation ((windowType -> Window.WouldPlaceBreach (LocationTarget lid)) : _) = lid
getWindowLocation (_ : rest) = getWindowLocation rest
getWindowLocation [] = error "No location id found"

instance RunMessage TheChariotVII where
  runMessage msg a@(TheChariotVII attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getBatchId -> batchId) _ -> do
      playerCount <- getPlayerCount
      locations <- sampleLocations (playerCount + 1)
      lead <- getLead
      push $ IgnoreBatch batchId
      chooseOneAtATimeM lead do
        targets locations \location -> push $ PlaceBreaches (toTarget location) 1
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 (getBatchId &&& getWindowLocation -> (batchId, lid)) _ -> do
      pushAll [IgnoreBatch batchId, Incursion lid]
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      anetteMasonIsPossessedByEvil <- getHasRecord AnetteMasonIsPossessedByEvil
      push $ if anetteMasonIsPossessedByEvil then R3 else R4
      pure a
    _ -> TheChariotVII <$> liftRunMessage msg attrs
