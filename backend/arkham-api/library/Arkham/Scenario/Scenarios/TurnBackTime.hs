module Arkham.Scenario.Scenarios.TurnBackTime (TurnBackTime (..), turnBackTime) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Log
import Arkham.Helpers.Scenario
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.TurnBackTime.Story
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype TurnBackTime = TurnBackTime ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

turnBackTime :: Difficulty -> TurnBackTime
turnBackTime difficulty =
  scenario
    TurnBackTime
    "04344"
    "Turn Back Time"
    difficulty
    [ ".        ancientHall  undergroundRuins .             .             .    ."
    , "entryway ancientHall  undergroundRuins secretPassage chamberOfTime .    ."
    , "entryway grandChamber burialPit        secretPassage chamberOfTime .    ."
    , ".        grandChamber burialPit        .             .             .    ."
    , "pos1     pos2         pos3             pos4          pos5          pos6 pos7"
    ]

instance HasChaosTokenValue TurnBackTime where
  getChaosTokenValue iid chaosTokenFace (TurnBackTime attrs) = case chaosTokenFace of
    Skull -> do
      locationsWithDoom <- selectCount LocationWithAnyDoom
      totalLocationDoom <- getSum <$> selectAgg Sum LocationDoom LocationWithAnyDoom
      pure $ toChaosTokenValue attrs Skull locationsWithDoom totalLocationDoom
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 4 6
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage TurnBackTime where
  runMessage msg s@(TurnBackTime attrs) = runQueueT $ case msg of
    Do (CheckWindows [Window Timing.When (Window.DrawingStartingHand iid) _]) -> do
      mRepossessThePast <- selectOne $ inDeckOf iid <> basic (cardIs Assets.relicOfAgesRepossessThePast)
      pushAll
        [ RemovePlayerCardFromGame True repossessThePast
        | repossessThePast <- maybeToList mRepossessThePast
        ]
      pure s
    PreScenarioSetup -> do
      forcedToWaitForSupplies <- getHasRecord TheInvestigatorsWereForcedToWaitForAdditionalSupplies
      doStep (if forcedToWaitForSupplies then 1 else 2) PreScenarioSetup
      pure s
    DoStep 1 PreScenarioSetup -> do
      story intro1
      traverse_ crossOut . drop 3 . campaignLogOrderedKeys =<< getCampaignLog
      pure s
    DoStep 2 PreScenarioSetup -> do
      story intro2
      traverse_ crossOut . drop 3 . campaignLogOrderedKeys =<< getCampaignLog
      pure s
    Setup -> runScenarioSetup TurnBackTime attrs do
      gather Set.TheDoomOfEztli
      gather Set.AgentsOfYig
      gather Set.YigsVenom
      gather Set.TemporalFlux
      gather Set.DeadlyTraps
      gather Set.ForgottenRuins
      gather Set.Poison
      gather Set.ChillingCold

      setAgendaDeck [Agendas.somethingStirs, Agendas.theTempleWarden]
      setActDeck [Acts.intoTheRuinsOnceAgain, Acts.theChamberOfStillRemains, Acts.momentOfDoom]

      startAt =<< place Locations.entryway

      addExtraDeck ExplorationDeck
        =<< shuffle
          [ Locations.ancientHall
          , Locations.grandChamber
          , Locations.burialPit
          , Locations.undergroundRuins
          , Locations.secretPassage
          , Treacheries.illOmen
          , Treacheries.deepDark
          , Treacheries.finalMistake
          , Treacheries.entombed
          , Treacheries.cryptChill
          ]

      setAsidePoisonedCount <- getSetAsidePoisonedCount
      setAside
        $ [ Locations.chamberOfTime
          , Assets.relicOfAgesRepossessThePast
          , Enemies.harbingerOfValusia
          ]
        <> replicate setAsidePoisonedCount Treacheries.poisoned

      addChaosToken ElderThing
    ResolveChaosToken _ ElderThing iid | isHardExpert attrs -> do
      mlid <- field InvestigatorLocation iid
      for_ mlid $ \lid -> placeDoom ElderThing lid 1
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        ElderThing | isEasyStandard attrs -> do
          mlid <- field InvestigatorLocation iid
          for_ mlid $ \lid -> placeDoom ElderThing lid 1
        _ -> pure ()
      pure s
    ScenarioResolution resolution -> do
      case resolution of
        NoResolution -> do
          record TheFabricOfTimeIsUnwoven
          eachInvestigator drivenInsane
          gameOver
        Resolution 1 -> do
          record TheInvestigatorsSealedTheRelicOfAgesForever
          allGainXp attrs
          gameOver
        _ -> error "Unknown Resolution"
      pure s
    Explore iid _ _ -> do
      checkWhen $ Window.AttemptExplore iid
      push $ Do msg
      pure s
    Do (Explore iid source locationMatcher) -> do
      explore iid source locationMatcher PlaceExplored 1
      pure s
    _ -> TurnBackTime <$> liftRunMessage msg attrs
