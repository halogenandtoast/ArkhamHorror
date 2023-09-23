module Arkham.Scenario.Scenarios.TheDepthsOfYoth (
  TheDepthsOfYoth (..),
  theDepthsOfYoth,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message hiding (EnemyDamage)
import Arkham.Placement
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TheDepthsOfYoth.Helpers
import Arkham.Scenarios.TheDepthsOfYoth.Story
import Arkham.Timing qualified as Timing
import Arkham.Token qualified as Token
import Arkham.Trait (Trait (Injury))
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Window (mkWindow)
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
    Skull -> do
      depth <- getCurrentDepth
      pure $ ChaosTokenValue Skull $ NegativeModifier depth
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
standaloneCampaignLog =
  mkCampaignLog {campaignLogRecorded = setFromList [TheRelicIsMissing]}

instance RunMessage TheDepthsOfYoth where
  runMessage msg s@(TheDepthsOfYoth attrs) = case msg of
    SetChaosTokensForScenario -> do
      whenM getIsStandalone $ push $ SetChaosTokens standaloneChaosTokens
      pure s
    StandaloneSetup -> do
      lead <- getLead
      push
        $ questionLabel
          "The investigators may choose how many tally marks are under “Yig’s Fury.” The lower the number chosen, the safer and easier the scenario will be."
          lead
        $ ChooseAmounts
          "Fury"
          (MaxAmountTarget 9000)
          [AmountChoice "Fury" 0 9000]
          (toTarget attrs)
      pure
        . TheDepthsOfYoth
        $ attrs
        & standaloneCampaignLogL
        .~ standaloneCampaignLog
    Setup -> do
      isStandalone <- getIsStandalone
      investigatorIds <- allInvestigatorIds

      yigsFury <- getRecordCount YigsFury

      let
        otherLocationCards =
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

      encounterDeck <-
        buildEncounterDeckExcluding
          ( Enemies.yig
              : Locations.stepsOfYoth
              : otherLocationCards
                <> [Enemies.pitWarden | yigsFury == 0]
          )
          [ EncounterSet.TheDepthsOfYoth
          , EncounterSet.AgentsOfYig
          , EncounterSet.YigsVenom
          , EncounterSet.Expedition
          , EncounterSet.ForgottenRuins
          , EncounterSet.Poison
          ]

      stepsOfYoth <- genCard Locations.stepsOfYoth
      locations <- shuffleM =<< genCards otherLocationCards

      let
        (startLocation, rest) = case locations of
          (x : xs) -> (x, xs)
          _ -> error "impossible"
        setAsideLocations = drop 4 rest
      explorationDeck <- shuffleM $ stepsOfYoth : take 4 rest

      (startLocationId, placeStartLocation) <- placeLocation startLocation

      forgingYourOwnPath <- getHasRecord YouAreForgingYourOwnWay
      ichtacasFaithIsRestored <- getHasRecord IchtacasFaithIsRestored
      theRelicIsMissing <- getHasRecord TheRelicIsMissing
      hasPocketknife <- getAnyHasSupply Pocketknife

      let
        isIntro2 = forgingYourOwnPath
        isIntro4 = not forgingYourOwnPath && not ichtacasFaithIsRestored
        isIntro6 = isIntro4 && not theRelicIsMissing
        isIntro8 = isIntro6 && not hasPocketknife
        startsOnAgenda5 = yigsFury >= 18
        startsOnAgenda6 = yigsFury >= 21

      yig <- genCard Enemies.yig
      harbingerOfValusia <- genCard Enemies.harbingerOfValusia

      createHarbinger <- createEnemyWithPlacement_ harbingerOfValusia (OutOfPlay PursuitZone)
      createYig <- createEnemyWithPlacement_ yig (OutOfPlay PursuitZone)

      pushAll
        $ story investigatorIds intro1
        : [story investigatorIds intro2 | isIntro2]
          <> [AddChaosToken ElderThing | isIntro2 && not isStandalone]
          <> [ story investigatorIds intro3
             | not forgingYourOwnPath && ichtacasFaithIsRestored
             ]
          <> [story investigatorIds intro4 | isIntro2]
          <> [Record IchtacaIsSetAgainstYou | isIntro2 || isIntro4]
          <> [RemoveCampaignCard Assets.ichtacaTheForgottenGuardian | isIntro4]
          <> [story investigatorIds intro5 | isIntro4 && theRelicIsMissing]
          <> [story investigatorIds intro6 | isIntro6]
          <> [story investigatorIds intro7 | isIntro6 && hasPocketknife]
          <> [story investigatorIds intro8 | isIntro8]
          <> [CrossOutRecord TheInvestigatorsFoundTheMissingRelic | isIntro8]
          <> [Record TheRelicIsMissing | isIntro8]
          <> [RemoveCampaignCard Assets.relicOfAgesADeviceOfSomeSort | isIntro4]
          <> [ RemoveCampaignCard Assets.relicOfAgesForestallingTheFuture
             | isIntro4
             ]
          <> [ SetEncounterDeck encounterDeck
             , SetAgendaDeck
             , SetActDeck
             , placeStartLocation
             , MoveAllTo (toSource attrs) startLocationId
             ]
          <> [createHarbinger | startsOnAgenda5]
          <> [createYig | startsOnAgenda6]

      setAsidePoisonedCount <- getSetAsidePoisonedCount
      theHarbingerIsStillAlive <- getHasRecord TheHarbingerIsStillAlive
      setAsideCards <-
        genCards
          $ Assets.relicOfAgesRepossessThePast
          : [ Enemies.harbingerOfValusia
            | theHarbingerIsStillAlive && not startsOnAgenda5
            ]
            <> [Enemies.yig | not startsOnAgenda6]
            <> replicate setAsidePoisonedCount Treacheries.poisoned

      acts <- genCards [Acts.journeyToTheNexus]
      agendas <-
        genCards
          $ [Agendas.theDescentBegins | yigsFury < 6] -- 1
          <> [Agendas.horrificDescent | yigsFury < 11] -- 2
          <> [Agendas.endlessCaverns | yigsFury < 15] -- 3
          <> [Agendas.cityOfBlood | yigsFury < 18] -- 4
          <> [Agendas.furyThatShakesTheEarth | yigsFury < 21] -- 5
          <> [Agendas.theRedDepths, Agendas.vengeance] -- 6,7
      TheDepthsOfYoth
        <$> runMessage
          msg
          ( attrs
              & (decksL . at ExplorationDeck ?~ explorationDeck)
              & (agendaStackL . at 1 ?~ agendas)
              & (actStackL . at 1 ?~ acts)
              & (setAsideCardsL .~ setAsideCards <> setAsideLocations)
              & (metaL .~ toMeta startLocationId)
              & (countsL .~ mapFromList [(CurrentDepth, 1)])
          )
    ResolveAmounts _ (getChoiceAmount "Fury" -> n) ScenarioTarget -> do
      push $ RecordCount YigsFury n
      pure s
    CreatedEnemyAt harbingerId _ (isTarget attrs -> True) -> do
      isHarbinger <- harbingerId <=~> enemyIs Enemies.harbingerOfValusia
      when isHarbinger $ do
        startingDamage <- getRecordCount TheHarbingerIsStillAlive
        when (startingDamage > 0)
          $ push
          $ PlaceTokens (toSource attrs) (toTarget harbingerId) Token.Damage startingDamage
      pure s
    Explore iid _ _ -> do
      windowMsg <- checkWindows [mkWindow Timing.When $ Window.AttemptExplore iid]
      pushAll [windowMsg, Do msg]
      pure s
    Do (Explore iid source locationMatcher) -> do
      explore iid source locationMatcher PlaceExplored 1
      pure s
    ScenarioResolution r -> do
      iids <- allInvestigatorIds
      depth <- getCurrentDepth
      gainXp <- toGainXp attrs $ getXpWithBonus depth
      vengeance <- getVengeanceInVictoryDisplay
      yigsFury <- getRecordCount YigsFury
      inVictory <-
        selectAny
          $ VictoryDisplayCardMatch
          $ cardIs
            Enemies.harbingerOfValusia
      inPlayHarbinger <- selectOne $ enemyIs Enemies.harbingerOfValusia
      damage <- case inPlayHarbinger of
        Just eid -> field EnemyDamage eid
        Nothing -> getRecordCount TheHarbingerIsStillAlive
      case r of
        NoResolution -> push $ ScenarioResolution (Resolution 1)
        Resolution n | n == 1 || n == 2 -> do
          currentDepth <- getCurrentDepth
          let
            resolution = if n == 1 then resolution1 else resolution2
            recordEntry =
              if n == 1
                then TheInvestigatorsFellIntoTheDepths
                else TheNexusIsNear
            depthMessages =
              if n > 1
                then []
                else case currentDepth of
                  1 -> map (InvestigatorKilled ScenarioSource) iids <> [GameOver]
                  2 ->
                    map (\i -> SufferTrauma i 2 0) iids
                      <> [ScenarioResolutionStep 1 (Resolution 1)]
                      <> map
                        ( \i ->
                            SearchCollectionForRandom i (toSource attrs)
                              $ WeaknessCard
                              <> CardWithTrait Injury
                        )
                        iids
                  3 ->
                    map (\i -> SufferTrauma i 1 0) iids
                      <> [ScenarioResolutionStep 1 (Resolution 1)]
                  _ -> []
          pushAll
            $ [story iids resolution, Record recordEntry]
            <> depthMessages
            <> [CrossOutRecord TheHarbingerIsStillAlive | inVictory]
            <> [RecordCount TheHarbingerIsStillAlive damage | not inVictory]
            <> [RecordCount YigsFury (yigsFury + vengeance)]
            <> gainXp
            <> [EndOfGame Nothing]
        _ -> error "Unknown Resolution"
      pure s
    ScenarioResolutionStep 1 (Resolution 1) -> do
      allKilled <- selectNone AliveInvestigator
      when allKilled $ push GameOver
      pure s
    RequestedPlayerCard iid (isSource attrs -> True) mcard _ -> do
      for_ mcard $ \card ->
        push
          $ ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) [PlayerCard card]
      pure s
    _ -> TheDepthsOfYoth <$> runMessage msg attrs
