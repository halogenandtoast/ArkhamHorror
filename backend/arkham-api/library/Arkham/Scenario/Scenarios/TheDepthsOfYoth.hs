module Arkham.Scenario.Scenarios.TheDepthsOfYoth (setupTheDepthsOfYoth, theDepthsOfYoth, TheDepthsOfYoth (..)) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLog
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Key
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Effect.Window
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Helpers.Xp
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message (questionLabel)
import Arkham.Message.Lifted.Log
import Arkham.Placement
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted hiding (EnemyDamage, getIsReturnTo, questionLabel)
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TheDepthsOfYoth.Helpers
import Arkham.Trait (Trait (Injury, Serpent))
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Window qualified as Window

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
standaloneCampaignLog = mkCampaignLog {campaignLogRecorded = setFromList [toCampaignLogKey TheRelicIsMissing]}

setupTheDepthsOfYoth :: (HasI18n, ReverseQueue m) => ScenarioAttrs -> ScenarioBuilderT m ()
setupTheDepthsOfYoth _attrs = do
  isReturnTo <- getIsReturnTo
  yigsFury <- getRecordCount YigsFury

  let
    startsOnAgenda5 = yigsFury >= 18
    startsOnAgenda6 = yigsFury >= 21
    harbinger = if isReturnTo then Enemies.harbingerOfValusiaTheSleeperReturns else Enemies.harbingerOfValusia

  setup $ ul do
    li "gatherSets"
    scope "yigsFury" $ li.nested "description" do
      li.validate (yigsFury == 0) "tally0"
      li.validate (yigsFury >= 1 && yigsFury <= 5) "tally1to5"
      li.validate (yigsFury >= 6 && yigsFury <= 10) "tally6to10"
      li.validate (yigsFury >= 11 && yigsFury <= 14) "tally11to14"
      li.validate (yigsFury >= 15 && yigsFury <= 17) "tally15to17"
      li.validate (yigsFury >= 18 && yigsFury <= 20) "tally18to20"
      li.validate (yigsFury >= 21) "tally21OrMore"
    li "placeLocations"
    scope "theHarbinger" $ li.nested "description" do
      li.validate (startsOnAgenda5 || startsOnAgenda6) "agenda5or6"
    scope "yig" $ li.nested "description" do
      li.validate startsOnAgenda6 "agenda6"
    li "explorationDeck"
    li "currentDepth"
    li "setOutOfPlay"
    li "poisoned"
    unscoped $ li "shuffleRemainder"

  scope "depthLevel" $ flavor $ h "title" >> p "body"
  scope "pursuit" $ flavor $ h "title" >> p "body"

  whenReturnTo $ gather Set.ReturnToTheDepthsOfYoth
  gather Set.TheDepthsOfYoth
  gather Set.AgentsOfYig
  gather Set.YigsVenom `orWhenReturnTo` gather Set.VenomousHate
  gather Set.Expedition `orWhenReturnTo` gather Set.DoomedExpedition
  gather Set.ForgottenRuins
  gather Set.Poison

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

  explorationDeck <-
    fromGathered $ mapOneOf cardIs $ [Treacheries.perilsOfYoth, Locations.stepsOfYoth] <> inExplore
  addExtraDeck ExplorationDeck =<< shuffle explorationDeck

  startLocation <- place start
  startAt startLocation
  setMeta (toMeta startLocation Nothing)
  setCount CurrentDepth 1

  when startsOnAgenda5 $ placeEnemy harbinger (OutOfPlay PursuitZone)
  when startsOnAgenda6 $ placeEnemy Enemies.yig (OutOfPlay PursuitZone)

  setAsidePoisonedCount <- getSetAsidePoisonedCount
  theHarbingerIsStillAlive <- getHasRecord TheHarbingerIsStillAlive

  setAside
    $ Assets.relicOfAgesRepossessThePast
    : [harbinger | theHarbingerIsStillAlive && not startsOnAgenda5]
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
  whenReturnTo do
    removeEvery [Enemies.harbingerOfValusia]
    addAdditionalReferences ["53059b"]
    createAbilityEffect EffectGameWindow
      $ mkAbility (SourceableWithCardCode (CardCode "53059b") ScenarioSource) 1
      $ forced
      $ ScenarioEvent #after Nothing "newExplorationDeck"

instance RunMessage TheDepthsOfYoth where
  runMessage msg s@(TheDepthsOfYoth attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      forgingYourOwnPath <- getHasRecord YouAreForgingYourOwnWay
      ichtacasFaithIsRestored <- getHasRecord IchtacasFaithIsRestored
      flavor do
        h "title"
        scope "intro1" do
          p "body"
          p.basic.right.validate forgingYourOwnPath "forgingYourOwnPath"
          p.basic.right.validate (not forgingYourOwnPath && ichtacasFaithIsRestored) "faithIsRestored"
          p.basic.right.validate (not forgingYourOwnPath && not ichtacasFaithIsRestored) "otherwise"
      doStep (if forgingYourOwnPath then 2 else if ichtacasFaithIsRestored then 3 else 4) PreScenarioSetup
      pure s
    DoStep 2 PreScenarioSetup -> scope "intro" do
      flavor $ h "title" >> p "intro2"
      record IchtacaIsSetAgainstYou
      unlessStandalone $ addChaosToken ElderThing
      pure s
    DoStep 3 PreScenarioSetup -> scope "intro" do
      flavor $ h "title" >> p "intro3"
      pure s
    DoStep 4 PreScenarioSetup -> scope "intro" do
      theRelicIsMissing <- getHasRecord TheRelicIsMissing
      flavor do
        h "title"
        scope "intro4" do
          p "body"
          p.basic.right.validate theRelicIsMissing "theRelicIsMissing"
          p.basic.right.validate (not theRelicIsMissing) "foundTheMissingRelic"
      record IchtacaIsSetAgainstYou
      removeCampaignCard Assets.ichtacaTheForgottenGuardian
      doStep (if theRelicIsMissing then 5 else 6) PreScenarioSetup
      pure s
    DoStep 5 PreScenarioSetup -> scope "intro" do
      flavor $ h "title" >> p "intro5"
      pure s
    DoStep 6 PreScenarioSetup -> scope "intro" do
      flavor $ h "title" >> p "intro6"
      hasPocketknife <- getAnyHasSupply Pocketknife
      doStep (if hasPocketknife then 7 else 8) PreScenarioSetup
      pure s
    DoStep 7 PreScenarioSetup -> scope "intro" do
      flavor $ h "title" >> p "intro7"
      pure s
    DoStep 8 PreScenarioSetup -> scope "intro" do
      flavor $ h "title" >> p "intro8"
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
    Setup -> runScenarioSetup TheDepthsOfYoth attrs $ setupTheDepthsOfYoth attrs
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
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          resolution "noResolution"
          push R1
        Resolution n | n == 1 || n == 2 -> do
          depth <- getCurrentDepth
          resolutionWithXp (if n == 1 then "resolution1" else "resolution2")
            $ allGainXpWithBonus' attrs (toBonus "depth" depth)

          record $ if n == 1 then TheInvestigatorsFellIntoTheDepths else TheNexusIsNear

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

          vengeance <- getTotalVengeanceInVictoryDisplay
          yigsFury <- getRecordCount YigsFury
          recordCount YigsFury (yigsFury + vengeance)

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
              <> oneOf [enemyAtLocationWith iid, EnemyAt $ connectedFrom (locationWithInvestigator iid)]
          for_ serpents \serpent -> do
            push $ HealDamage (toTarget serpent) (ChaosTokenEffectSource Cultist) 2
        Tablet -> withLocationOf iid \location -> placeTokens Tablet location #clue 2
        _ -> pure ()
      pure s
    UseCardAbility _ ScenarioSource 1 _ _ -> do
      outOfPlayPerilsOfYoth <- getSetAsideCardsMatching $ cardIs Treacheries.perilsOfYoth
      inDiscardPerilsOfYoth <-
        map toCard
          . filterCards (cardIs Treacheries.perilsOfYoth)
          <$> getEncounterDiscard RegularEncounterDeck
      inDeckPerilsOfYoth <-
        map toCard . filterCards (cardIs Treacheries.perilsOfYoth) . unDeck <$> getEncounterDeck
      let perilsOfYoth = outOfPlayPerilsOfYoth <> inDiscardPerilsOfYoth <> inDeckPerilsOfYoth
      unless (null perilsOfYoth) $ shuffleCardsIntoDeck ExplorationDeck perilsOfYoth
      unless (null inDeckPerilsOfYoth) $ shuffleDeck Deck.EncounterDeck
      pure s
    _ -> TheDepthsOfYoth <$> liftRunMessage msg attrs
