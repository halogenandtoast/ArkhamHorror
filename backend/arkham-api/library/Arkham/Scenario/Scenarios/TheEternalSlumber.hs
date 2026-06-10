module Arkham.Scenario.Scenarios.TheEternalSlumber (theEternalSlumber, TheEternalSlumber (..)) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Campaigns.GuardiansOfTheAbyss.Helpers
import Arkham.Campaigns.TheForgottenAge.Helpers (ExploreRule (PlaceExplored), explore)
import Arkham.Helpers.Location (getLocationOf)
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query (allInvestigators, getPlayerCount)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TheEternalSlumber.Helpers
import Arkham.SkillType
import Arkham.Window qualified as Window

newtype TheEternalSlumber = TheEternalSlumber ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theEternalSlumber :: Difficulty -> TheEternalSlumber
theEternalSlumber difficulty =
  sideStory
    TheEternalSlumber
    "83001"
    "The Eternal Slumber"
    difficulty
    [ ".      trefoil  .       .         .    ."
    , "square equals   diamond circle    .    ."
    , ".      triangle moon    hourglass plus ."
    , ".      star     heart   droplet   t    squiggle"
    ]

{- FOURMOLU_DISABLE -}
standardTokens, hardTokens :: [ChaosTokenFace]
standardTokens =
  [ PlusOne , PlusOne , Zero , Zero , MinusOne , MinusOne , MinusOne , MinusTwo , MinusTwo
  , MinusThree , MinusThree , MinusFour , MinusSix , Skull , Skull , Skull , Cultist , Tablet
  , ElderThing , AutoFail , ElderSign
  ]
hardTokens =
  [ PlusOne , Zero , Zero , MinusOne , MinusOne , MinusOne , MinusTwo , MinusTwo , MinusTwo
  , MinusThree , MinusThree , MinusFour , MinusFour , MinusFive , MinusSeven , Skull , Skull
  , Skull , Cultist , Tablet , ElderThing , AutoFail , ElderSign
  ]
{- FOURMOLU_ENABLE -}

instance HasChaosTokenValue TheEternalSlumber where
  getChaosTokenValue iid tokenFace (TheEternalSlumber attrs) = case tokenFace of
    Skull -> do
      n <- getStrengthOfTheAbyss
      pure $ ChaosTokenValue Skull $ NegativeModifier $ if isEasyStandard attrs then n else n + 1
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ toChaosTokenValue attrs Tablet 0 2
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 5 6
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage TheEternalSlumber where
  runMessage msg s@(TheEternalSlumber attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      flavor $ h "title" >> p "body"
      pure s
    StandaloneSetup -> do
      setChaosTokens $ if isEasyStandard attrs then standardTokens else hardTokens
      pure s
    Setup -> runScenarioSetup TheEternalSlumber attrs do
      setup $ ul do
        li "gatherSets"
        li.nested "placeLocations" do
          li "startAt"
        li "setAside"
        li "brotherhood"
        li "strengthOfTheAbyss"
        unscoped $ li "shuffleRemainder"
        unscoped $ li "readyToBegin"

      gather Set.TheEternalSlumber
      gather Set.SandsOfEgypt
      gather Set.BrotherhoodOfTheBeast
      gather Set.AbyssalTribute

      setAgendaDeck [Agendas.jessiesRequest, Agendas.curseOfTheAbyss, Agendas.gardenOfShadows]
      setActDeck [Acts.curseOfEndlessSleep, Acts.secretsInTheSand, Acts.theHourOfJudgment]

      startAt =<< place Locations.streetsOfCairo
      placeAll
        [ Locations.outskirtsOfCairo
        , Locations.templeCourtyard
        , Locations.museumOfEgyptianAntiquities
        , Locations.cairoBazaar
        , Locations.expeditionCampGuardiansOfTheAbyss
        ]

      setAside
        [ Enemies.neith
        , Assets.johnAndJessieBurke
        , Assets.ancientAnkh
        , Enemies.abyssalRevenant
        , Enemies.abyssalRevenant
        , Locations.nileRiver
        , Locations.sandsOfDashur
        , Locations.dunesOfTheSahara
        , Locations.untouchedVault
        , Locations.facelessSphinx
        , Locations.desertOasis
        , Locations.sandsweptRuins
        ]

      placeUnderScenarioReference =<< shuffle brotherhoodEnemies

      n <- getPlayerCount
      push $ ScenarioCountSet StrengthOfTheAbyss n
    Explore iid source _ -> do
      mloc <- runMaybeT $ asum [hoistMaybe source.location, MaybeT $ getLocationOf iid]
      checkWhen $ Window.AttemptExplore iid mloc
      do_ msg
      pure s
    Do (Explore iid source locationMatcher) -> do
      explore iid source locationMatcher PlaceExplored 1
      pure s
    ResolveChaosToken _ Cultist iid -> do
      drawAnotherChaosToken iid
      pure s
    ResolveChaosToken _ Tablet iid -> do
      withSkillTest \sid -> do
        skillTestModifier sid Tablet sid CancelSkills
        skillTestModifiers sid Tablet iid $ map SkillCannotBeIncreased allSkills
      pure s
    ResolveChaosToken _ ElderThing iid -> do
      chooseOneM iid do
        labeled' "elderThing.addStrength" do
          addStrengthOfTheAbyss 1
          passSkillTest
        unscoped skip_
      pure s
    FailedSkillTest _ _ _ (ChaosTokenTarget token) _ _ -> do
      when (token.face == Cultist) do
        n <- getStrengthOfTheAbyss
        let threshold = if isEasyStandard attrs then 3 else 5
        when (n <= threshold) $ addStrengthOfTheAbyss 1
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          resolutionWithXp "noResolution" $ allGainXp' attrs
          recordCountM DreamersInTheAbyss getStrengthOfTheAbyss
          recordBrotherhoodAgentsWhoEscaped
          record TheBrotherhoodsSchemesContinueUnabated
          killRemainingTakenByTheAbyss ScenarioSource
          endOfScenario
        Resolution 1 -> do
          record TheCurseOfSlumberWasLifted
          crossOutTakenByTheAbyss
          investigators <- allInvestigators
          addCampaignCardToDeckChoice investigators DoNotShuffleIn Assets.johnAndJessieBurke
          resolutionWithXp "resolution1" $ allGainXp' attrs
          recordCountM DreamersInTheAbyss getStrengthOfTheAbyss
          recordBrotherhoodAgentsWhoEscaped
          endOfScenario
        Resolution 2 -> do
          record YouAreAwareOfXzharahsPlans
          investigators <- allInvestigators
          addCampaignCardToDeckChoice investigators DoNotShuffleIn Assets.ancientAnkh
          resolutionWithXp "resolution2" $ allGainXp' attrs
          recordCountM DreamersInTheAbyss getStrengthOfTheAbyss
          recordBrotherhoodAgentsWhoEscaped
          killRemainingTakenByTheAbyss ScenarioSource
          endOfScenario
        _ -> error "invalid resolution"
      pure s
    _ -> TheEternalSlumber <$> liftRunMessage msg attrs
