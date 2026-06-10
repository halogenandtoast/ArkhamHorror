module Arkham.Scenario.Scenarios.TheNightsUsurper (theNightsUsurper, TheNightsUsurper (..)) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card (toCardCode)
import Arkham.CampaignLogKey
import Arkham.Campaigns.GuardiansOfTheAbyss.Helpers
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query (allInvestigators, getPlayerCount)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TheNightsUsurper.Helpers
import Arkham.SkillType
import Arkham.Treachery.Cards qualified as Treacheries

newtype TheNightsUsurper = TheNightsUsurper ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theNightsUsurper :: Difficulty -> TheNightsUsurper
theNightsUsurper difficulty =
  sideStory
    TheNightsUsurper
    "83016"
    "The Night's Usurper"
    difficulty
    [ "nileRiver      sandsOfDashur stairwayToSarkomand dunesOfTheSahara untouchedVault"
    , "facelessSphinx desertOasis   expeditionCamp      sandsweptRuins   eldritchGate"
    , ".              tunnelsUnderNgranek theGreatAbyss mistFilledCaverns ."
    , ".              .             aDreamBetwixt       .                ."
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

instance HasChaosTokenValue TheNightsUsurper where
  getChaosTokenValue iid tokenFace (TheNightsUsurper attrs) = case tokenFace of
    Skull -> do
      n <- getStrengthOfTheAbyss
      pure $ ChaosTokenValue Skull $ NegativeModifier $ if isEasyStandard attrs then n else n + 1
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ toChaosTokenValue attrs Tablet 1 3
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 4 5
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage TheNightsUsurper where
  runMessage msg s@(TheNightsUsurper attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      flavor $ h "title" >> p "body"
      pure s
    StandaloneSetup -> do
      setChaosTokens $ if isEasyStandard attrs then standardTokens else hardTokens
      pure s
    Setup -> runScenarioSetup TheNightsUsurper attrs do
      setup $ ul do
        li "gatherSets"
        li "placeLocations"
        li "setAside"
        li "explorationDeck"
        li.nested "brotherhood" do
          li "brotherhoodCampaign"
          li "brotherhoodStandalone"
        li.nested "strengthOfTheAbyss" do
          li "strengthCampaign"
          li "strengthStandalone"
        unscoped $ li "shuffleRemainder"
        unscoped $ li "readyToBegin"

      gather Set.TheNightsUsurper
      gather Set.SandsOfEgypt
      gather Set.BrotherhoodOfTheBeast
      gather Set.AbyssalGifts

      setAgendaDeck [Agendas.theBrotherhoodBidesTheirTime, Agendas.schemesInTheDarkBeyond]
      setActDeck [Acts.searchForTheGate, Acts.intoTheGate, Acts.theNightsUsurper]

      startAt =<< place Locations.expeditionCampGuardiansOfTheAbyss

      setAside
        [ Assets.khopeshOfTheAbyss
        , Assets.summonedNightgaunt
        , Locations.eldritchGate
        , Enemies.xzharah
        , Locations.aDreamBetwixt
        , Locations.theGreatAbyss
        , Locations.tunnelsUnderNgranek
        , Locations.stairwayToSarkomand
        , Locations.mistFilledCaverns
        , Enemies.dreadedShantak
        , Enemies.dreadedShantak
        ]

      explorationLocations <-
        fromGathered
          $ mapOneOf
            cardIs
            [ Locations.nileRiver
            , Locations.sandsOfDashur
            , Locations.dunesOfTheSahara
            , Locations.untouchedVault
            , Locations.facelessSphinx
            , Locations.desertOasis
            , Locations.sandsweptRuins
            ]
      explorationTreacheries <- for
        [ Treacheries.abyssalReach
        , Treacheries.theBlackWind
        , Treacheries.terrorUnderThePyramids
        , Treacheries.swarmOfLocusts
        ]
        \def -> do
          cards <- take 1 <$> amongGathered (cardIs def)
          removeCards cards
          pure cards
      addExtraDeck ExplorationDeck
        =<< shuffle (explorationLocations <> concat explorationTreacheries)

      standalone <- getIsStandalone
      if standalone
        then do
          shuffled <- shuffle brotherhoodEnemies
          placeUnderScenarioReference (take 3 shuffled)
          removeEvery (drop 3 shuffled)
        else do
          escaped <- recordedCardCodes <$> getRecordSet BrotherhoodAgentsWhoEscaped
          let (kept, removed) = partition ((`elem` escaped) . toCardCode) brotherhoodEnemies
          placeUnderScenarioReference kept
          removeEvery removed

      n <-
        if standalone
          then getPlayerCount
          else getRecordCount DreamersInTheAbyss
      push $ ScenarioCountSet StrengthOfTheAbyss n
    ResolveChaosToken _ Cultist iid -> do
      drawAnotherChaosToken iid
      pure s
    ResolveChaosToken _ Tablet iid -> do
      withSkillTest \sid -> do
        skillTestModifier sid Tablet sid CancelSkills
        skillTestModifiers sid Tablet iid $ map SkillCannotBeIncreased allSkills
      pure s
    PassedSkillTest _ _ _ (ChaosTokenTarget token) _ _ -> do
      when (token.face == ElderThing) do
        n <- getStrengthOfTheAbyss
        when (n >= 4) $ removeStrengthOfTheAbyss 1
      pure s
    FailedSkillTest _ _ _ (ChaosTokenTarget token) _ _ -> do
      when (token.face == Cultist) do
        n <- getStrengthOfTheAbyss
        let threshold = if isEasyStandard attrs then 3 else 5
        when (n <= threshold) $ addStrengthOfTheAbyss 1
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> push R1
        Resolution 1 -> do
          record TheDayOfReckoningIsComing
          resolutionWithXp "resolution1" $ allGainXp' attrs
          killRemainingTakenByTheAbyss ScenarioSource
          endOfScenario
        Resolution 2 -> do
          record TheAbyssWasSaved
          crossOutTakenByTheAbyss
          investigators <- allInvestigators
          addCampaignCardToDeckChoice investigators DoNotShuffleIn Assets.summonedNightgaunt
          resolutionWithXp "resolution2" $ allGainXp' attrs
          endOfScenario
        Resolution 3 -> do
          record YouJoinedForcesWithXzharah
          investigators <- allInvestigators
          addCampaignCardToDeckChoice investigators DoNotShuffleIn Assets.khopeshOfTheAbyss
          resolutionWithXp "resolution3" $ allGainXp' attrs
          killRemainingTakenByTheAbyss ScenarioSource
          endOfScenario
        _ -> error "invalid resolution"
      pure s
    _ -> TheNightsUsurper <$> liftRunMessage msg attrs
