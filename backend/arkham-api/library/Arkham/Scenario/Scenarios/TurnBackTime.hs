module Arkham.Scenario.Scenarios.TurnBackTime (setupTurnBackTime, turnBackTime, TurnBackTime (..)) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLog
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Key
import Arkham.Card.CardCode
import Arkham.Effect.Window
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Log
import Arkham.Helpers.Scenario
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message.Lifted.Log
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted hiding (getIsReturnTo)
import Arkham.Scenarios.TurnBackTime.Helpers
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
    [ ".      square diamond  .        .         .    ."
    , "circle square diamond  squiggle hourglass .    ."
    , "circle star   triangle squiggle hourglass .    ."
    , ".      star   triangle .        .         .    ."
    , "pos1   pos2   pos3     pos4     pos5      pos6 pos7"
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

setupTurnBackTime :: (HasI18n, ReverseQueue m) => ScenarioAttrs -> ScenarioBuilderT m ()
setupTurnBackTime _attrs = do
  setup do
    ul do
      li "gatherSets"
      li "actDeck"
      li "relicOfAgesRepossessThePast"
      li "relicOfAgesADeviceOfSomeSort"
      li "placeLocations"
      li "explorationDeck"
      li "setAside"
      li "chaosBag"
      li "poisoned"
      unscoped $ li "shuffleRemainder"

  whenReturnTo do
    gather Set.ReturnToTurnBackTime
    gather Set.ReturnToTheDoomOfEztli
  gather Set.TheDoomOfEztli
  gather Set.AgentsOfYig
  gather Set.YigsVenom `orWhenReturnTo` gather Set.VenomousHate
  gather Set.TemporalFlux `orWhenReturnTo` gather Set.TemporalHunters
  gather Set.DeadlyTraps
  gather Set.ForgottenRuins
  gather Set.Poison
  gather Set.ChillingCold

  setAgendaDeck [Agendas.somethingStirs, Agendas.theTempleWarden]
  setActDeck [Acts.intoTheRuinsOnceAgain, Acts.theChamberOfStillRemains, Acts.momentOfDoom]

  isReturnTo <- getIsReturnTo
  startAt =<< place (if isReturnTo then Locations.entrywayRearrangedByTime else Locations.entryway)

  -- Before setup, replace the original Chamber of Time locations with the new
  -- versions from this encounter set.
  addExtraDeck ExplorationDeck
    =<< shuffle
      ( [ if isReturnTo then Locations.ancientHallRearrangedByTime else Locations.ancientHall
        , if isReturnTo then Locations.grandChamberRearrangedByTime else Locations.grandChamber
        ]
          <> ( guard isReturnTo
                 *> [ Locations.snakePit
                    , Locations.throneRoom
                    , Locations.tombOfTheAncients
                    , Locations.mosaicChamber
                    , Locations.sealedPassage
                    ]
             )
          <> ( guard (not isReturnTo)
                 *> [ Treacheries.illOmen
                    , Treacheries.deepDark
                    , Treacheries.finalMistake
                    , Treacheries.entombed
                    , Treacheries.cryptChill
                    , Locations.burialPit
                    , Locations.undergroundRuins
                    , Locations.secretPassage
                    ]
             )
      )

  setAsidePoisonedCount <- getSetAsidePoisonedCount
  let harbinger = if isReturnTo then Enemies.harbingerOfValusiaTheSleeperReturns else Enemies.harbingerOfValusia
  setAside
    $ [ if isReturnTo then Locations.chamberOfTimeRearrangedByTime else Locations.chamberOfTime
      , Assets.relicOfAgesRepossessThePast
      , harbinger
      ]
    <> replicate setAsidePoisonedCount Treacheries.poisoned
    <> (guard isReturnTo *> [Enemies.pitViper, Enemies.pitViper, Enemies.pitViper])

  addChaosToken ElderThing

  whenReturnTo do
    removeEvery
      [ Enemies.harbingerOfValusia
      , Locations.ancientHall
      , Locations.burialPit
      , Locations.entryway
      , Locations.grandChamber
      , Locations.undergroundRuins
      , Locations.secretPassage
      , Locations.chamberOfTime
      ]
    addAdditionalReferences ["53066b"]
    createAbilityEffect EffectGameWindow
      $ mkAbility (SourceableWithCardCode (CardCode "53066b") ScenarioSource) 1
      $ forced
      $ Explored #after Anyone Anywhere (SuccessfulExplore Anywhere)

instance RunMessage TurnBackTime where
  runMessage msg s@(TurnBackTime attrs) = runQueueT $ scenarioI18n $ case msg of
    Do (CheckWindows [Window Timing.When (Window.DrawingStartingHand iid) _]) -> do
      mRepossessThePast <- selectOne $ inDeckOf iid <> basic (cardIs Assets.relicOfAgesRepossessThePast)
      pushAll
        [ RemovePlayerCardFromGame True repossessThePast
        | repossessThePast <- maybeToList mRepossessThePast
        ]
      pure s
    PreScenarioSetup -> scope "intro" do
      forcedToWaitForSupplies <- getHasRecord TheInvestigatorsWereForcedToWaitForAdditionalSupplies
      flavor do
        setTitle "title"
        p.validate forcedToWaitForSupplies "forcedToWaitForSupplies"
        p.validate (not forcedToWaitForSupplies) "clearedAPathToTheEztliRuins"
      doStep (if forcedToWaitForSupplies then 1 else 2) PreScenarioSetup
      pure s
    DoStep 1 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "intro1"
      traverse_ crossOut . drop 3 . campaignLogOrderedKeys =<< getCampaignLog
      pure s
    DoStep 2 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "intro2"
      traverse_ crossOut . drop 3 . campaignLogOrderedKeys =<< getCampaignLog
      pure s
    Setup -> runScenarioSetup TurnBackTime attrs $ setupTurnBackTime attrs
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
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          resolution "noResolution"
          record TheFabricOfTimeIsUnwoven
          eachInvestigator drivenInsane
          gameOver
        Resolution 1 -> do
          resolutionWithXp "resolution1" $ allGainXp' attrs
          record TheInvestigatorsSealedTheRelicOfAgesForever
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
    UseCardAbility _ ScenarioSource 1 _ _ -> do
      getEncounterDeck >>= \case
        Deck [] -> pure ()
        Deck (x : _) -> shuffleCardsIntoDeck ExplorationDeck [x]
      pure s
    _ -> TurnBackTime <$> liftRunMessage msg attrs
