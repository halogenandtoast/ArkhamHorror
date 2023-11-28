module Arkham.Act.Cards.JourneyToTheNexus (JourneyToTheNexus (..), journeyToTheNexus) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Classes
import Arkham.Helpers.Investigator
import Arkham.Helpers.Location
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (InvestigatorDefeated, LocationCard)
import Arkham.Placement
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Scenarios.TheDepthsOfYoth.Helpers

newtype JourneyToTheNexus = JourneyToTheNexus ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasAbilities JourneyToTheNexus where
  getAbilities (JourneyToTheNexus a) =
    [ restrictedAbility a 1 (exists $ YourLocation <> LocationWithoutClues) exploreAction_
    , mkAbility a 2
        $ Objective
        $ ReactionAbility (RoundEnds #when)
        $ GroupClueCost (PerPlayer 3) "Steps of Yoth"
    ]

journeyToTheNexus :: ActCard JourneyToTheNexus
journeyToTheNexus = act (1, A) JourneyToTheNexus Cards.journeyToTheNexus Nothing

instance RunMessage JourneyToTheNexus where
  runMessage msg a@(JourneyToTheNexus attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locationSymbols <- toConnections =<< getJustLocation iid
      let source = toAbilitySource attrs 1
      push $ Explore iid source (oneOf $ map CardWithPrintedLocationSymbol locationSymbols)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ advanceVia #clues a iid
      pure a
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      pushAll [NextAdvanceActStep (toId attrs) 1, NextAdvanceActStep (toId attrs) 2]
      pure a
    NextAdvanceActStep aid 1 | aid == toId attrs -> do
      msgs <- incrementDepth
      pushAll msgs
      pure a
    NextAdvanceActStep aid 2 | aid == toId attrs -> do
      depth <- getCurrentDepth
      isStandalone <- getIsStandalone
      if depth >= 5 && not isStandalone
        then push R2
        else do
          defeated <- selectList $ NotInvestigator $ InvestigatorAt $ locationIs Locations.stepsOfYoth
          enemies <- selectList AnyEnemy
          explorationDeck <- getExplorationDeck
          stepsOfYoth <- selectJust $ locationIs Locations.stepsOfYoth
          stepsOfYothCard <- field LocationCard stepsOfYoth
          otherLocations <- selectList $ NotLocation $ LocationWithId stepsOfYoth
          locationCards <- traverse (field LocationCard) otherLocations
          let notStepsOfYoth = locationCards <> explorationDeck
          (newStart, rest) <- do
            shuffled <- shuffleM notStepsOfYoth
            case shuffled of
              [] -> error "no locations"
              (x : xs) -> pure (x, xs)
          newExplorationDeck <- shuffleM (stepsOfYothCard : rest)
          (newStartId, placeNewStart) <- placeLocation newStart
          pushAll
            $ map (InvestigatorDefeated (toSource attrs)) defeated
            <> map (InvestigatorDiscardAllClues (toSource attrs)) defeated
            <> map (\e -> PlaceEnemy e (OutOfPlay PursuitZone)) enemies
            <> map
              (RemoveAllDoom (toSource attrs) . LocationTarget)
              (stepsOfYoth : otherLocations)
            <> map RemoveLocation otherLocations
            <> [ placeNewStart
               , MoveAllTo (toSource attrs) newStartId
               , RemoveLocation stepsOfYoth
               , SetScenarioDeck ExplorationDeck newExplorationDeck
               , SetScenarioMeta $ toMeta newStartId
               , RevertAct $ toId attrs
               ]
      pure a
    RevertAct aid | aid == toId attrs && onSide B attrs -> do
      pure $ JourneyToTheNexus $ attrs & (sequenceL .~ Sequence 1 A)
    _ -> JourneyToTheNexus <$> runMessage msg attrs
