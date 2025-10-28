module Arkham.Act.Cards.JourneyToTheNexus (journeyToTheNexus) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Act.Sequence
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Card
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (InvestigatorDefeated, LocationCard)
import Arkham.Message.Lifted.Move
import Arkham.Message.Lifted.Placement
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Scenarios.TheDepthsOfYoth.Helpers
import Arkham.Window qualified as Window

newtype JourneyToTheNexus = JourneyToTheNexus ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasAbilities JourneyToTheNexus where
  getAbilities (JourneyToTheNexus a) =
    [ restricted a 1 (exists $ YourLocation <> LocationWithoutClues) exploreAction_
    , mkAbility a 2
        $ Objective
        $ triggered (RoundEnds #when)
        $ GroupClueCost (PerPlayer 3) "Steps of Yoth"
    ]

journeyToTheNexus :: ActCard JourneyToTheNexus
journeyToTheNexus = act (1, A) JourneyToTheNexus Cards.journeyToTheNexus Nothing

instance RunMessage JourneyToTheNexus where
  runMessage msg a@(JourneyToTheNexus attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      runExplore iid (attrs.ability 1)
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      advancedWithClues attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      incrementDepth
      doStep 2 msg
      pure a
    DoStep 2 msg'@(AdvanceAct (isSide B attrs -> True) _ _) -> do
      depth <- getCurrentDepth
      isStandalone <- getIsStandalone
      if depth >= 5 && not isStandalone
        then push R2
        else do
          selectEach
            (investigator_ $ not_ $ at_ $ locationIs Locations.stepsOfYoth)
            (investigatorDefeated attrs)
          investigators <- select $ investigator_ $ at_ $ locationIs Locations.stepsOfYoth
          for_ investigators \iid -> do
            discardAllClues attrs iid
            place iid Unplaced

          selectEach AnyInPlayEnemy \e -> place e (OutOfPlay PursuitZone)

          inPlayLocations <- select Anywhere
          locationCards <- for inPlayLocations \lid -> do
            removeAllDoom attrs lid
            removeLocation lid
            field LocationCard lid
          (explorationLocations, explorationOther) <-
            partition (`cardMatch` card_ #location) <$> getExplorationDeck

          for_ (explorationLocations <> locationCards) setCardAside
          addToEncounterDiscard explorationOther
          setScenarioDeck ExplorationDeck []

          doStep 3 msg'
      pure a
    DoStep 3 (AdvanceAct (isSide B attrs -> True) _ _) -> do
      setAsideLocations <-
        shuffle =<< getSetAsideCardsMatching (#location <> NotCard (cardIs Locations.stepsOfYoth))
      let
        newStart :| rest = case nonEmpty setAsideLocations of
          Nothing -> error "no locations"
          Just xs -> xs

      newStartId <- placeLocation newStart
      moveAllTo attrs newStartId

      stepsOfYoth <- fetchCard Locations.stepsOfYoth
      setScenarioDeck ExplorationDeck =<< shuffle (stepsOfYoth : take 4 rest)

      msource <- getCurrentExploreSource
      setScenarioMeta $ toMeta newStartId msource
      checkAfter $ Window.ScenarioEvent "newExplorationDeck" Nothing Null
      push $ RevertAct $ toId attrs
      pure a
    RevertAct (isSide B attrs -> True) -> do
      pure $ JourneyToTheNexus $ attrs & (sequenceL .~ Sequence 1 A)
    _ -> JourneyToTheNexus <$> liftRunMessage msg attrs
