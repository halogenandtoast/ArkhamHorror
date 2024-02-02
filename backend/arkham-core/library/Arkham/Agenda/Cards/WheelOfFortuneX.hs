module Arkham.Agenda.Cards.WheelOfFortuneX (
  WheelOfFortuneX (..),
  wheelOfFortuneX,
) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheCircleUndone.Memento
import Arkham.Card
import Arkham.Classes
import Arkham.Direction
import Arkham.GameValue
import Arkham.Helpers
import Arkham.Helpers.Investigator
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Movement
import Arkham.Projection
import Arkham.Scenarios.BeforeTheBlackThrone.Cosmos
import Arkham.Scenarios.BeforeTheBlackThrone.Helpers
import Arkham.Treachery.Cards qualified as Treacheries

newtype Metadata = Metadata {locationsMoved :: [LocationId]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, NoThunks, NFData)

newtype WheelOfFortuneX = WheelOfFortuneX (AgendaAttrs `With` Metadata)
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

wheelOfFortuneX :: AgendaCard WheelOfFortuneX
wheelOfFortuneX =
  agendaWith (1, A) (WheelOfFortuneX . (`with` Metadata [])) Cards.wheelOfFortuneX (Static 4)
    $ removeDoomMatchersL
    %~ (\m -> m {removeDoomEnemies = NotEnemy AnyEnemy})

instance RunMessage WheelOfFortuneX where
  runMessage msg a@(WheelOfFortuneX (attrs `With` meta)) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
        ritualSuicideMessages <- commitRitualSuicide attrs
        lead <- getLead
        ableToFindYourWay <- Gilman'sJournal `inRecordSet` MementosDiscovered
        investigators <- getInvestigators
        pushAll
          $ ritualSuicideMessages
          <> [findAndDrawEncounterCard lead (cardIs Treacheries.daemonicPiping)]
          <> ( guard (not ableToFindYourWay)
                *> [ForInvestigator iid (NextAdvanceAgendaStep (toId attrs) 1) | iid <- investigators]
             )
          <> [advanceAgendaDeck attrs]

        pure a
      ForInvestigator iid (NextAdvanceAgendaStep aid _) | aid == toId attrs -> do
        location <- getJustLocation iid
        mpos <- findLocationInCosmos location
        player <- getPlayer iid
        for_ mpos $ \pos -> do
          mLeftLocation <- getLocationInDirection pos GridLeft
          canMoveLocationLeft <-
            (&&) (location `notElem` locationsMoved meta) <$> getCanMoveLocationLeft location

          pushWhen (isJust mLeftLocation || canMoveLocationLeft)
            $ chooseOrRunOne player
            $ [ Label "Move to the location to your left" [Move $ move attrs iid leftLocation]
              | leftLocation <- maybeToList mLeftLocation
              ]
            <> [ Label
                "Move the placement of your location once to the left"
                [HandleTargetChoice iid (toSource attrs) (toTarget location)]
               | canMoveLocationLeft
               ]

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
                (emptySpace', placeEmptySpace) <- placeLocationCard Locations.emptySpace
                pushAll
                  [ ObtainCard card
                  , placeEmptySpace
                  , PlaceCosmos iid emptySpace' (EmptySpace pos card)
                  , PlaceCosmos iid lid (CosmosLocation (updatePosition pos GridLeft) lid)
                  ]
                pure a
          [] -> error "empty deck, what should we do?, maybe don't let this be called?"
          _ -> error "too many cards, why did this happen?"
      _ -> WheelOfFortuneX . (`with` meta) <$> runMessage msg attrs
