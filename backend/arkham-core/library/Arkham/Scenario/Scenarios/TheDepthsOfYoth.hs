module Arkham.Scenario.Scenarios.TheDepthsOfYoth
  ( TheDepthsOfYoth(..)
  , theDepthsOfYoth
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
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message
import Arkham.Placement
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TheDepthsOfYoth.Helpers
import Arkham.Scenarios.TheDepthsOfYoth.Story
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window

newtype TheDepthsOfYoth = TheDepthsOfYoth ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theDepthsOfYoth :: Difficulty -> TheDepthsOfYoth
theDepthsOfYoth difficulty = scenario
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

instance HasTokenValue TheDepthsOfYoth where
  getTokenValue iid tokenFace (TheDepthsOfYoth attrs) = case tokenFace of
    Skull -> do
      depth <- getCurrentDepth
      pure $ TokenValue Skull $ NegativeModifier depth
    Cultist -> pure $ TokenValue Cultist NoModifier
    Tablet -> pure $ TokenValue Tablet NoModifier
    ElderThing -> pure $ toTokenValue attrs ElderThing 2 4
    otherFace -> getTokenValue iid otherFace attrs

standaloneTokens :: [TokenFace]
standaloneTokens =
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
  mkCampaignLog { campaignLogRecorded = setFromList [TheRelicIsMissing] }

instance RunMessage TheDepthsOfYoth where
  runMessage msg s@(TheDepthsOfYoth attrs) = case msg of
    SetTokensForScenario -> do
      whenM getIsStandalone $ push $ SetTokens standaloneTokens
      pure s
    StandaloneSetup -> do
      leadInvestigatorId <- getLeadInvestigatorId
      push
        $ Ask leadInvestigatorId
        $ QuestionLabel
            "The investigators may choose how many tally marks are under “Yig’s Fury.” The lower the number chosen, the safer and easier the scenario will be."
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

      encounterDeck <- buildEncounterDeckExcluding
        (Enemies.yig
        : Locations.stepsOfYoth
        : otherLocationCards
        <> [ Enemies.pitWarden | yigsFury == 0 ]
        )
        [ EncounterSet.TheDepthsOfYoth
        , EncounterSet.AgentsOfYig
        , EncounterSet.YigsVenom
        , EncounterSet.Expedition
        , EncounterSet.ForgottenRuins
        , EncounterSet.Poison
        ]

      stepsOfYoth <- genCard Locations.stepsOfYoth
      locations <- shuffleM =<< traverse genCard otherLocationCards

      let
        (startLocation, rest) = case locations of
          (x : xs) -> (x, xs)
          _ -> error "impossible"
      explorationDeck <- shuffleM $ stepsOfYoth : take 4 rest

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

      pushAll
        $ story investigatorIds intro1
        : [ story investigatorIds intro2 | isIntro2 ]
        <> [ AddToken ElderThing | isIntro2 && not isStandalone ]
        <> [ story investigatorIds intro3
           | not forgingYourOwnPath && ichtacasFaithIsRestored
           ]
        <> [ story investigatorIds intro4 | isIntro2 ]
        <> [ Record IchtacaIsSetAgainstYou | isIntro2 || isIntro4 ]
        <> [ RemoveCampaignCard Assets.ichtacaTheForgottenGuardian | isIntro4 ]
        <> [ story investigatorIds intro5 | isIntro4 && theRelicIsMissing ]
        <> [ story investigatorIds intro6 | isIntro6 ]
        <> [ story investigatorIds intro7 | isIntro6 && hasPocketknife ]
        <> [ story investigatorIds intro8 | isIntro8 ]
        <> [ CrossOutRecord TheInvestigatorsFoundTheMissingRelic | isIntro8 ]
        <> [ Record TheRelicIsMissing | isIntro8 ]
        <> [ RemoveCampaignCard Assets.relicOfAgesADeviceOfSomeSort | isIntro4 ]
        <> [ RemoveCampaignCard Assets.relicOfAgesForestallingTheFuture
           | isIntro4
           ]
        <> [ SetEncounterDeck encounterDeck
           , SetAgendaDeck
           , SetActDeck
           , PlaceLocation startLocation
           , MoveAllTo (toSource attrs) (toLocationId startLocation)
           ]
        <> [ CreateEnemyWithPlacement harbingerOfValusia Pursuit
           | startsOnAgenda5
           ]
        <> [ CreateEnemyWithPlacement yig Pursuit | startsOnAgenda6 ]

      setAsidePoisonedCount <- getSetAsidePoisonedCount
      theHarbingerIsStillAlive <- getHasRecord TheHarbingerIsStillAlive
      setAsideCards <-
        traverse genCard
        $ Assets.relicOfAgesRepossessThePast
        : [ Enemies.harbingerOfValusia
          | theHarbingerIsStillAlive && not startsOnAgenda5
          ]
        <> [ Enemies.yig | not startsOnAgenda6 ]
        <> replicate setAsidePoisonedCount Treacheries.poisoned

      TheDepthsOfYoth <$> runMessage
        msg
        (attrs
        & (decksL . at ExplorationDeck ?~ explorationDeck)
        & (agendaStackL
          . at 1
          ?~ [ Agendas.theDescentBegins | yigsFury < 6 ] -- 1
          <> [ Agendas.horrificDescent | yigsFury < 11 ] -- 2
          <> [ Agendas.endlessCaverns | yigsFury < 15 ] -- 3
          <> [ Agendas.cityOfBlood | yigsFury < 18 ] -- 4
          <> [ Agendas.furyThatShakesTheEarth | yigsFury < 21 ] -- 5
          <> [Agendas.theRedDepths, Agendas.vengeance] -- 6,7
          )
        & (actStackL . at 1 ?~ [Acts.journeyToTheNexus])
        & (setAsideCardsL .~ setAsideCards)
        & (metaL .~ toMeta (toLocationId startLocation))
        & (countsL .~ mapFromList [(CurrentDepth, 1)])
        )
    ResolveAmounts _ (getChoiceAmount "Fury" -> n) ScenarioTarget -> do
      push $ RecordCount YigsFury n
      pure s
    CreatedEnemyAt harbingerId _ (isTarget attrs -> True) -> do
      isHarbinger <- harbingerId <=~> enemyIs Enemies.harbingerOfValusia
      when isHarbinger $ do
        startingDamage <- getRecordCount TheHarbingerIsStillAlive
        when (startingDamage > 0) $ push $ PlaceDamage
          (EnemyTarget harbingerId)
          startingDamage
      pure s
    Explore iid _ _ -> do
      windowMsg <- checkWindows [Window Timing.When $ Window.AttemptExplore iid]
      pushAll [windowMsg, Do msg]
      pure s
    Do (Explore iid source locationMatcher) -> do
      explore iid source locationMatcher PlaceExplored 1
      pure s
    _ -> TheDepthsOfYoth <$> runMessage msg attrs
