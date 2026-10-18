module Arkham.Homebrew.AgesUnwound.Scenarios.NightOfFire (nightOfFire) where

import Arkham.Helpers.FlavorText (additionalRules, flavor, h, li, p, setup, ul)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Helpers.Xp (toBonus)
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.AgesUnwound.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.Key
import Arkham.Homebrew.AgesUnwound.ScenarioDeckKeys (pattern ArkhamStreetsDeck)
import Arkham.Homebrew.AgesUnwound.Scenarios.NightOfFire.Helpers
import Arkham.Homebrew.AgesUnwound.Sets qualified as Set
import Arkham.Matcher
import Arkham.Message.Lifted.Log (record)
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted

newtype NightOfFire = NightOfFire ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nightOfFire :: Difficulty -> NightOfFire
nightOfFire difficulty =
  scenario
    NightOfFire
    ":ages-unwound:001"
    "Night of Fire"
    difficulty
    [ ".      moon     triangle"
    , "circle square   t"
    , "plus   diamond  hourglass"
    ]

instance HasChaosTokenValue NightOfFire where
  getChaosTokenValue iid tokenFace (NightOfFire attrs) = case tokenFace of
    Skull -> do
      n <- length <$> getArkhamStreetsDeck
      pure $ toChaosTokenValue attrs Skull (n `div` 2) n
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> do
      moved <- iid <=~> InvestigatorThatMovedDuringTurn
      pure
        $ if moved
          then toChaosTokenValue attrs Tablet 1 2
          else toChaosTokenValue attrs Tablet 3 4
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage NightOfFire where
  runMessage msg s@(NightOfFire attrs) = runQueueT $ nightOfFireI18n $ case msg of
    PreScenarioSetup -> do
      flavor $ scope "intro" $ h "title" >> p "body"
      pure s
    Setup -> runScenarioSetup NightOfFire attrs do
      setup $ ul do
        li "gatherSets"
        li "placeRivertown"
        li "arkhamStreetsDeck"
        li "setAside"
        unscoped $ li "shuffleRemainder"
        unscoped $ li "readyToBegin"

      additionalRules "arkhamStreets"

      gather Set.NightOfFire
      gather Set.Nyctophobia
      gather Set.Thugs
      gather Set.UnravellingYears
      gatherAndSetAside Set.AgentsOfAforgomon

      setAside
        [ Enemies.myriadAssassin
        , Enemies.irregulars
        , Enemies.eternitysSentinel_016
        , Enemies.eternitysSentinel_017
        ]
      setAsideEvery $ cardIs Enemies.timeSpirit

      setAgendaDeck [Agendas.hunted, Agendas.watched, Agendas.gazeOfThreeEyes]
      setActDeck [Acts.onTheLam, Acts.gettingYourBearings, Acts.slippingTheNet]

      startAt =<< place Locations.rivertown

      addExtraDeck ArkhamStreetsDeck
        =<< shuffle
          [ Locations.independenceSquare
          , Locations.easttown
          , Locations.frenchHill
          , Locations.twistingAlleys
          , Locations.winchmoreHouse
          , Locations.crampedPassage
          , Locations.aPlaceToHide
          ]
    ResolveChaosToken _ Cultist iid -> do
      drawAnotherChaosToken iid
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ | token.face == Cultist -> do
      withSkillTest \sid ->
        afterThisTestResolves sid $ moveToNewArkhamStreetsLocation Cultist iid
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          resolution "noResolution"
          push R1
        Resolution 1 -> do
          record YourHuntersFoundANewQuarry
          resolutionWithXp "resolution1" $ allGainXp' attrs
          endOfScenario
        Resolution 2 -> do
          record TheInvestigatorsSurvivedTheNightOfFire
          resolutionWithXp "resolution2"
            $ allGainXpWithBonus' attrs (toBonus "desperateFlight" 2)
          endOfScenario
        Resolution 3 -> do
          record TheInvestigatorsSlewTheirStrangeObserver
          resolutionWithXp "resolution3" $ allGainXp' attrs
          endOfScenario
        Resolution 4 -> do
          record TheInvestigatorsEscapedTheNightOfFire
          resolutionWithXp "resolution4"
            $ allGainXpWithBonus' attrs (toBonus "desperateFlight" 2)
          endOfScenario
        _ -> error $ "Unknown resolution: " <> show r
      pure s
    _ -> NightOfFire <$> liftRunMessage msg attrs
