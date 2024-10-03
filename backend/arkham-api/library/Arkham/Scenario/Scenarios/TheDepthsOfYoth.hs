module Arkham.Scenario.Scenarios.TheDepthsOfYoth (TheDepthsOfYoth (..), theDepthsOfYoth) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Investigator (withLocationOf)
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message (getChoiceAmount, questionLabel)
import Arkham.Placement
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Helpers hiding (checkWhen)
import Arkham.Scenario.Import.Lifted hiding (EnemyDamage, questionLabel)
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TheDepthsOfYoth.Helpers
import Arkham.Scenarios.TheDepthsOfYoth.Story
import Arkham.Trait (Trait (Injury, Serpent))
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Window qualified as Window
import Arkham.Zone

newtype TheDepthsOfYoth = TheDepthsOfYoth ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theDepthsOfYoth :: Difficulty -> TheDepthsOfYoth
theDepthsOfYoth difficulty =
  scenario
    TheDepthsOfYoth
    "04277"
    "The Depths of Yoth"
    difficulty
    [ ".        diamond . .         droplet  ."
    , "equals   .       . .         .        heart"
    , ".        .       . hourglass .        ."
    , ".        .       t .         .        ."
    , "triangle .       . .         .        circle"
    , ".        square  . .         squiggle ."
    ]

instance HasChaosTokenValue TheDepthsOfYoth where
  getChaosTokenValue iid chaosTokenFace (TheDepthsOfYoth attrs) = case chaosTokenFace of
    Skull -> ChaosTokenValue Skull . NegativeModifier <$> getCurrentDepth
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 2 4
    otherFace -> getChaosTokenValue iid otherFace attrs

standaloneChaosTokens :: [ChaosTokenFace]
standaloneChaosTokens =
  [ PlusOne
  , Zero
  , Zero
  , Zero
  , MinusOne
  , MinusTwo
  , MinusTwo
  , MinusThree
  , MinusFive
  , Skull
  , Skull
  , Cultist
  , ElderThing
  , AutoFail
  , ElderSign
  ]

standaloneCampaignLog :: CampaignLog
standaloneCampaignLog = mkCampaignLog {campaignLogRecorded = setFromList [TheRelicIsMissing]}

instance RunMessage TheDepthsOfYoth where
  runMessage msg s@(TheDepthsOfYoth attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      story intro1
      forgingYourOwnPath <- getHasRecord YouAreForgingYourOwnWay
      ichtacasFaithIsRestored <- getHasRecord IchtacasFaithIsRestored
      doStep (if forgingYourOwnPath then 2 else if ichtacasFaithIsRestored then 3 else 4) PreScenarioSetup
      pure s
    DoStep 2 PreScenarioSetup -> do
      story intro2
      record IchtacaIsSetAgainstYou
      unlessStandalone $ addChaosToken ElderThing
      pure s
    DoStep 3 PreScenarioSetup -> do
      story intro3
      pure s
    DoStep 4 PreScenarioSetup -> do
      story intro4
      record IchtacaIsSetAgainstYou
      removeCampaignCard Assets.ichtacaTheForgottenGuardian
      theRelicIsMissing <- getHasRecord TheRelicIsMissing
      doStep (if theRelicIsMissing then 5 else 6) PreScenarioSetup
      pure s
    DoStep 5 PreScenarioSetup -> do
      story intro5
      pure s
    DoStep 6 PreScenarioSetup -> do
      story intro6
      hasPocketknife <- getAnyHasSupply Pocketknife
      doStep (if hasPocketknife then 7 else 8) PreScenarioSetup
      pure s
    DoStep 7 PreScenarioSetup -> do
      story intro7
      pure s
    DoStep 8 PreScenarioSetup -> do
      story intro8
      crossOut TheInvestigatorsFoundTheMissingRelic
      record TheRelicIsMissing
      removeCampaignCard Assets.relicOfAgesADeviceOfSomeSort
      removeCampaignCard Assets.relicOfAgesForestallingTheFuture
      pure s
    StandaloneSetup -> do
      setChaosTokens standaloneChaosTokens
      lead <- getLeadPlayer
      choiceId <- getRandom
      push
        $ questionLabel
          "The investigators may choose how many tally marks are under “Yig’s Fury.” The lower the number chosen, the safer and easier the scenario will be."
          lead
        $ ChooseAmounts
          "Fury"
          (MaxAmountTarget 9000)
          [AmountChoice choiceId "Fury" 0 9000]
          (toTarget attrs)
      pure . TheDepthsOfYoth $ attrs & standaloneCampaignLogL .~ standaloneCampaignLog
    Setup -> runScenarioSetup TheDepthsOfYoth attrs do
      gather Set.TheDepthsOfYoth
      gather Set.AgentsOfYig
      gather Set.YigsVenom
      gather Set.Expedition
      gather Set.ForgottenRuins
      gather Set.Poison

      yigsFury <- getRecordCount YigsFury
      when (yigsFury == 0) $ removeEvery [Enemies.pitWarden, Enemies.yig]

      locations <-
        shuffleM
          [ Locations.cityOfTheSerpents
          , Locations.hallOfHeresy
          , Locations.crumblingPrecipice
          , Locations.cavernsOfYoth
          , Locations.forkedPath
          , Locations.bridgeOverNKai
          , Locations.brokenPassage
          , Locations.abandonedSite
          , Locations.brightCanyon
          ]

      let
        (start, rest) = case locations of
          (x : xs) -> (x, xs)
          _ -> error "impossible"
        (inExplore, setAsideLocations) = splitAt 4 rest

      addExtraDeck ExplorationDeck =<< shuffle (Locations.stepsOfYoth : inExplore)

      startLocation <- place start
      startAt startLocation
      setMeta (toMeta startLocation)
      setCount CurrentDepth 1

      let
        startsOnAgenda5 = yigsFury >= 18
        startsOnAgenda6 = yigsFury >= 21

      when startsOnAgenda5 $ placeEnemy Enemies.harbingerOfValusia (OutOfPlay PursuitZone)
      when startsOnAgenda6 $ placeEnemy Enemies.yig (OutOfPlay PursuitZone)

      setAsidePoisonedCount <- getSetAsidePoisonedCount
      theHarbingerIsStillAlive <- getHasRecord TheHarbingerIsStillAlive

      setAside
        $ Assets.relicOfAgesRepossessThePast
        : [ Enemies.harbingerOfValusia
          | theHarbingerIsStillAlive && not startsOnAgenda5
          ]
          <> [Enemies.yig | not startsOnAgenda6 && yigsFury > 0]
          <> replicate setAsidePoisonedCount Treacheries.poisoned
          <> setAsideLocations

      setActDeck [Acts.journeyToTheNexus]
      setAgendaDeck
        $ [Agendas.theDescentBegins | yigsFury < 6] -- 1
        <> [Agendas.horrificDescent | yigsFury < 11] -- 2
        <> [Agendas.endlessCaverns | yigsFury < 15] -- 3
        <> [Agendas.cityOfBlood | yigsFury < 18] -- 4
        <> [Agendas.furyThatShakesTheEarth | yigsFury < 21] -- 5
        <> [Agendas.theRedDepths, Agendas.vengeance] -- 6,7
    ResolveAmounts _ (getChoiceAmount "Fury" -> n) ScenarioTarget -> do
      recordCount YigsFury n
      pure s
    CreatedEnemyAt harbingerId _ (isTarget attrs -> True) -> do
      isHarbinger <- harbingerId <=~> enemyIs Enemies.harbingerOfValusia
      when isHarbinger do
        startingDamage <- getRecordCount TheHarbingerIsStillAlive
        when (startingDamage > 0) $ placeTokens attrs harbingerId #damage startingDamage
      pure s
    Explore iid _ _ -> do
      checkWhen $ Window.AttemptExplore iid
      push $ Do msg
      pure s
    Do (Explore iid source locationMatcher) -> do
      explore iid source locationMatcher PlaceExplored 1
      pure s
    ScenarioResolution r -> do
      case r of
        NoResolution -> push R1
        Resolution n | n == 1 || n == 2 -> do
          story $ if n == 1 then resolution1 else resolution2
          record $ if n == 1 then TheInvestigatorsFellIntoTheDepths else TheNexusIsNear

          depth <- getCurrentDepth
          unless (n > 1) do
            case depth of
              1 -> do
                eachInvestigator (kill attrs)
                gameOver
              2 -> do
                eachInvestigator (`sufferPhysicalTrauma` 2)
                push $ ScenarioResolutionStep 1 (Resolution 1)
                eachInvestigator \i -> searchCollectionForRandomBasicWeakness i attrs [Injury]
              3 -> do
                eachInvestigator (`sufferPhysicalTrauma` 1)
                push $ ScenarioResolutionStep 1 (Resolution 1)
              _ -> pure ()

          inVictory <- selectAny $ VictoryDisplayCardMatch $ basic $ cardIs Enemies.harbingerOfValusia
          if inVictory
            then crossOut TheHarbingerIsStillAlive
            else do
              damage <-
                selectOne (enemyIs Enemies.harbingerOfValusia) >>= \case
                  Just eid -> field EnemyDamage eid
                  Nothing -> getRecordCount TheHarbingerIsStillAlive
              recordCount TheHarbingerIsStillAlive damage

          vengeance <- getVengeanceInVictoryDisplay
          yigsFury <- getRecordCount YigsFury
          recordCount YigsFury (yigsFury + vengeance)

          allGainXpWithBonus attrs $ toBonus "depth" depth
          endOfScenario
        _ -> error "Unknown Resolution"
      pure s
    ScenarioResolutionStep 1 (Resolution 1) -> do
      allKilled <- selectNone AliveInvestigator
      when allKilled gameOver
      pure s
    RequestedPlayerCard iid (isSource attrs -> True) mcard _ -> do
      for_ mcard \card -> shuffleCardsIntoDeck iid [PlayerCard card]
      pure s
    ResolveChaosToken _ Cultist iid -> do
      drawAnotherChaosToken iid
      pure s
    ResolveChaosToken _ Tablet iid -> do
      drawAnotherChaosToken iid
      pure s
    ResolveChaosToken _ ElderThing _ -> do
      n <- getVengeanceInVictoryDisplay
      when (n >= 3) failSkillTest
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case chaosTokenFace token of
        Skull | isHardExpert attrs -> assignHorror iid Skull 1
        Cultist -> do
          serpents <-
            select
              $ EnemyWithTrait Serpent
              <> oneOf [enemyAtLocationWith iid, EnemyAt $ ConnectedFrom (locationWithInvestigator iid)]
          for_ serpents \serpent -> do
            push $ HealDamage (toTarget serpent) (ChaosTokenEffectSource Cultist) 2
        Tablet -> withLocationOf iid \location -> placeTokens Tablet location #clue 2
        _ -> pure ()
      pure s
    _ -> TheDepthsOfYoth <$> liftRunMessage msg attrs
