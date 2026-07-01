module Arkham.Scenario.Scenarios.OneLastJob (oneLastJob) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Campaigns.TheDrownedCity.Import
import Arkham.ChaosToken
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Location (connectBothWays)
import Arkham.Helpers.Xp
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
    [ "northside            downtown             easttown"
    , "tillinghastEsoterica miskatonicUniversity rivertown"
    ]

instance HasChaosTokenValue OneLastJob where
  getChaosTokenValue iid chaosTokenFace (OneLastJob attrs) = case chaosTokenFace of
    Skull -> do
      criminals <- selectCount $ EnemyWithTrait Criminal
      let value = byDifficulty attrs (min 4 criminals) (1 + criminals)
      pure $ ChaosTokenValue Skull (NegativeModifier value)
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
      setChaosTokens (chaosBagContents attrs.difficulty)
      pure s
    PreScenarioSetup -> do
      flavor $ scope "intro" do
        setTitle "title"
        p "body"
      pure s
    Setup -> runScenarioSetup OneLastJob attrs do
      setup $ ul do
        li "gatherSets"
        li "placeLocations"
        li "startAt"
        li "actDeck"
        li "setAside"
        unscoped $ li "shuffleRemainder"

      gather Set.OneLastJob
      gather Set.Dreams
      gather Set.ChillingCold
      gather Set.LockedDoors
      gather Set.Rats
      gather Set.StrikingFear

      -- One of the two (identical-front) Act 1 copies, chosen at random, decides
      -- which gang the investigators raid (its back puts the hideout into play).
      act1 <- sample2 Acts.questioningTheGangsV1 Acts.questioningTheGangsV2
      setActDeck [act1]
      setAside
        [ Acts.theSheldonGang
        , Acts.theOBannionGang
        , Acts.faceTheMusic
        ]

      setAgendaDeck [Agendas.arkhamNightlife, Agendas.longNight]

      -- Midnight Masks city districts (locations only); the removed locations are
      -- simply never placed.
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
      case res of
        NoResolution -> do
          record RubyWonTheBet
          resolutionWithXp "noResolution" $ allGainXpWithBonus' attrs $ toBonus "bonus" 0
        Resolution 1 -> do
          record RubyLostTheBet
          resolutionWithXp "resolution1" $ allGainXpWithBonus' attrs $ toBonus "bonus" 0
        _ -> error $ "Unknown resolution: " <> show res
      discoveredAlienLanguage <- getHasRecord TheInvestigatorsDiscoveredAnAlienLanguage
      when discoveredAlienLanguage do
        campaignSpecific "translateGlyph" ("F" :: Text, "Depths" :: Text)
      endOfScenario
      pure s
    _ -> OneLastJob <$> liftRunMessage msg attrs
