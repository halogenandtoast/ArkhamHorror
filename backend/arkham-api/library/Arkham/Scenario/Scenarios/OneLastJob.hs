module Arkham.Scenario.Scenarios.OneLastJob (oneLastJob) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Campaigns.TheDrownedCity.Import
import Arkham.ChaosToken
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Location (connectBothWays)
import Arkham.I18n
import Arkham.Investigator.Cards (wendyAdams, wendyAdamsParallel)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted hiding (setActDeck, setAgendaDeck)
import Arkham.Message.Lifted.Log
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.OneLastJob.Helpers
import Arkham.Trait (Trait (Criminal))

newtype OneLastJob = OneLastJob ScenarioAttrs
  deriving stock Generic
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

oneLastJob :: Difficulty -> OneLastJob
oneLastJob difficulty =
  scenario
    OneLastJob
    "11501"
    "One Last Job"
    difficulty
    [ ".                    laBellaLuna          hibbsRoadhouse"
    , "northside            downtown             easttown"
    , "tillinghastEsoterica miskatonicUniversity rivertown"
    ]

instance HasChaosTokenValue OneLastJob where
  getChaosTokenValue iid chaosTokenFace (OneLastJob attrs) = case chaosTokenFace of
    Skull -> do
      criminals <- selectCount $ EnemyWithTrait Criminal
      pure
        $ ChaosTokenValue Skull
        $ NegativeModifier
        $ byDifficulty attrs (min 4 criminals) (1 + criminals)
    Tablet -> do
      criminalHere <- selectAny $ EnemyWithTrait Criminal <> enemyAtLocationWith iid
      pure
        $ if criminalHere
          then toChaosTokenValue attrs Tablet 4 5
          else toChaosTokenValue attrs Tablet 2 3
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 4
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage OneLastJob where
  runMessage msg s@(OneLastJob attrs) = runQueueT $ scenarioI18n $ case msg of
    StandaloneSetup -> do
      setChaosTokens $ chaosBagContents attrs.difficulty
      pure s
    PreScenarioSetup -> do
      wendy <- selectAny $ mapOneOf investigatorIs [wendyAdams, wendyAdamsParallel]
      flavor $ scope "intro" do
        h "title"
        p "body1"
        p.validate wendy "wendy"
        p "body2"
      pure s
    Setup -> runScenarioSetup OneLastJob attrs do
      setup $ ul do
        li "gatherSets"
        li.nested "locations" do
          li "placeLocations"
          li "startAt"
        li "actDeck"
        li "setAside"
        unscoped $ li "shuffleRemainder"
        unscoped $ li "readyToBegin"

      gather Set.OneLastJob
      gather Set.Dreams
      gather Set.ChillingCold
      gather Set.LockedDoors
      gather Set.Rats
      gather Set.StrikingFear

      act1 <- sample2 Acts.questioningTheGangsV1 Acts.questioningTheGangsV2
      setActDeck [act1]
      setAside
        [ Acts.theSheldonGang
        , Acts.theOBannionGang
        , Acts.faceTheMusic
        ]

      setAgendaDeck [Agendas.arkhamNightlife, Agendas.longNight]

      placeAll
        [ Locations.northside
        , Locations.downtownFirstBankOfArkham
        , Locations.easttown
        , Locations.rivertown
        ]
      miskatonic <- place Locations.miskatonicUniversity
      tillinghast <- place Locations.tillinghastEsoterica
      connectBothWays tillinghast miskatonic
      startAt tillinghast

      setAside
        [ Enemies.sadieSheldon
        , Enemies.naomiOBannion
        , Enemies.gangEnforcer
        , Enemies.gangEnforcer
        ]
    ScenarioResolution res -> scope "resolutions" do
      discoveredAnAlienLanguage <- getHasRecord TheInvestigatorsDiscoveredAnAlienLanguage
      when discoveredAnAlienLanguage do
        campaignSpecific "translateGlyph" ("rune_a" :: Text, "Depths" :: Text)
      xp <- allGainXp' attrs
      case res of
        NoResolution -> scope "noResolution" do
          record RubyWonTheBet
          resolutionFlavor do
            setTitle "title"
            p "body"
            popScope $ ul do
              li "rubyLostTheBet"
              withXp xp $ li "victory"
              li.validate discoveredAnAlienLanguage "discoveredAnAlienLanguage"
              li "proceed"
        Resolution 1 -> scope "resolution1" do
          record RubyLostTheBet
          resolutionFlavor do
            setTitle "title"
            p "body"
            popScope $ ul do
              li "rubyWonTheBet"
              withXp xp $ li "victory"
              li.validate discoveredAnAlienLanguage "discoveredAnAlienLanguage"
              li "proceed"
        _ -> error $ "Unknown resolution: " <> show res
      endOfScenario
      pure s
    _ -> OneLastJob <$> liftRunMessage msg attrs
