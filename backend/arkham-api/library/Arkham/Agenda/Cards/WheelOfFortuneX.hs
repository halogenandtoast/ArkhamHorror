module Arkham.Agenda.Cards.WheelOfFortuneX (wheelOfFortuneX) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheCircleUndone.Memento
import Arkham.Campaigns.TheCircleUndone.Key
import Arkham.Card
import Arkham.Direction
import Arkham.Helpers
import Arkham.Helpers.Investigator
import Arkham.Helpers.Log (inRecordSet)
import Arkham.Helpers.Query (getLead)
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Projection
import Arkham.Scenarios.BeforeTheBlackThrone.Cosmos
import Arkham.Scenarios.BeforeTheBlackThrone.Helpers
import Arkham.Treachery.Cards qualified as Treacheries

newtype Metadata = Metadata {locationsMoved :: [LocationId]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype WheelOfFortuneX = WheelOfFortuneX (AgendaAttrs `With` Metadata)
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wheelOfFortuneX :: AgendaCard WheelOfFortuneX
wheelOfFortuneX =
  agendaWith (1, A) (WheelOfFortuneX . (`with` Metadata [])) Cards.wheelOfFortuneX (Static 4)
    $ removeDoomMatchersL
    %~ (\m -> m {removeDoomEnemies = NotEnemy AnyEnemy})

instance RunMessage WheelOfFortuneX where
  runMessage msg a@(WheelOfFortuneX (attrs `With` meta)) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      commitRitualSuicide attrs

      lead <- getLead
      findAndDrawEncounterCard lead (cardIs Treacheries.daemonicPiping)

      ableToFindYourWay <- Gilman'sJournal `inRecordSet` MementosDiscovered
      unless ableToFindYourWay do
        eachInvestigator (`forInvestigator` msg)
      advanceAgendaDeck attrs

      pure a
    ForInvestigator iid (AdvanceAgenda (isSide B attrs -> True)) -> do
      location <- getJustLocation iid
      mpos <- findLocationInCosmos location
      for_ mpos $ \pos -> do
        mLeftLocation <- getLocationInDirection pos GridLeft
        canMoveLocationLeft <-
          (&&) (location `notElem` locationsMoved meta) <$> getCanMoveLocationLeft location

        when (isJust mLeftLocation || canMoveLocationLeft) do
          chooseOrRunOneM iid do
            for_ mLeftLocation $ labeled "Move to the location to your left" . moveTo attrs iid
            when canMoveLocationLeft do
              labeled "Move the placement of your location once to the left" do
                handleTarget iid attrs location

      pure a
    HandleTargetChoice iid (isSource attrs -> True) (LocationTarget lid) -> do
      (map toCard -> cards, _) <- fieldMap InvestigatorDeck (draw 1) iid
      case cards of
        [card] -> do
          cosmos <- getCosmos
          let mpos = findInCosmos lid cosmos
          case mpos of
            Nothing -> error "location not found in cosmos, we shouldn't be here"
            Just pos -> do
              obtainCard card
              emptySpace' <- placeLocationCard Locations.emptySpace
              pushAll
                [ PlaceCosmos iid emptySpace' (EmptySpace pos card)
                , PlaceCosmos iid lid (CosmosLocation (updatePosition pos GridLeft) lid)
                ]
              pure a
        [] -> error "empty deck, what should we do?, maybe don't let this be called?"
        _ -> error "too many cards, why did this happen?"
    _ -> WheelOfFortuneX . (`with` meta) <$> liftRunMessage msg attrs
