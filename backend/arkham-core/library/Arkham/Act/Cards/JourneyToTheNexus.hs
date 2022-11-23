module Arkham.Act.Cards.JourneyToTheNexus
  ( JourneyToTheNexus(..)
  , journeyToTheNexus
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Action qualified as Action
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Card
import Arkham.Classes
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Helpers.Location
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types ( Field (..) )
import Arkham.Matcher hiding ( InvestigatorDefeated )
import Arkham.Message
import Arkham.Placement
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenarios.TheDepthsOfYoth.Helpers
import Arkham.Source
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype JourneyToTheNexus = JourneyToTheNexus ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasAbilities JourneyToTheNexus where
  getAbilities (JourneyToTheNexus a) =
    [ restrictedAbility
        a
        1
        (LocationExists $ YourLocation <> LocationWithoutClues)
      $ ActionAbility (Just Action.Explore)
      $ ActionCost 1
    , mkAbility a 2
      $ Objective
      $ ReactionAbility (RoundEnds Timing.When)
      $ GroupClueCost (PerPlayer 3) (LocationWithTitle "Steps of Yoth")
    ]

journeyToTheNexus :: ActCard JourneyToTheNexus
journeyToTheNexus =
  act (1, A) JourneyToTheNexus Cards.journeyToTheNexus Nothing

instance RunMessage JourneyToTheNexus where
  runMessage msg a@(JourneyToTheNexus attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      locationSymbols <- toConnections =<< getJustLocation iid
      push $ Explore
        iid
        (toSource attrs)
        (CardWithOneOf $ map CardWithPrintedLocationSymbol locationSymbols)
      pure a
    UseCardAbility iid source 2 _ _ | isSource attrs source ->
      a <$ push (AdvanceAct (toId a) (InvestigatorSource iid) AdvancedWithClues)
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      pushAll
        [NextAdvanceActStep (toId attrs) 1, NextAdvanceActStep (toId attrs) 2]
      pure a
    NextAdvanceActStep aid 1 | aid == toId attrs -> do
      msgs <- incrementDepth
      pushAll msgs
      pure a
    NextAdvanceActStep aid 2 | aid == toId attrs -> do
      depth <- getCurrentDepth
      isStandalone <- getIsStandalone
      if depth >= 5 && not isStandalone
        then push $ ScenarioResolution $ Resolution 2
        else do
          defeated <- selectList $ NotInvestigator $ InvestigatorAt $ locationIs
            Locations.stepsOfYoth
          enemies <- selectList AnyEnemy
          explorationDeck <- getExplorationDeck
          stepsOfYoth <- selectJust $ locationIs Locations.stepsOfYoth
          stepsOfYothCard <- field LocationCard stepsOfYoth
          otherLocations <- selectList $ NotLocation $ LocationWithId
            stepsOfYoth
          locationCards <- traverse (field LocationCard) otherLocations
          let notStepsOfYoth = locationCards <> explorationDeck
          (newStart, rest) <- do
            shuffled <- shuffleM notStepsOfYoth
            case shuffled of
              [] -> error "no locations"
              (x : xs) -> pure (x, xs)
          newExplorationDeck <- shuffleM (stepsOfYothCard : rest)
          pushAll
            $ map (InvestigatorDefeated (toSource attrs)) defeated
            <> map InvestigatorDiscardAllClues defeated
            <> map (\e -> PlaceEnemy e Pursuit) enemies
            <> map
                 (RemoveAllDoom . LocationTarget)
                 (stepsOfYoth : otherLocations)
            <> map RemoveLocation otherLocations
            <> [ PlaceLocation newStart
               , MoveAllTo (toSource attrs) (toLocationId newStart)
               , RemoveLocation stepsOfYoth
               , SetScenarioDeck ExplorationDeck newExplorationDeck
               , SetScenarioMeta $ toMeta depth (toLocationId newStart)
               , RevertAct $ toId attrs
               ]
      pure a
    RevertAct aid | aid == toId attrs && onSide B attrs ->
      pure $ JourneyToTheNexus $ attrs & (sequenceL .~ Sequence 1 A)
    _ -> JourneyToTheNexus <$> runMessage msg attrs
