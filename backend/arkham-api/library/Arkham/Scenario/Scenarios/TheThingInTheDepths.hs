module Arkham.Scenario.Scenarios.TheThingInTheDepths (theThingInTheDepths) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Campaigns.TheFeastOfHemlockVale.Key
import Arkham.Card
import Arkham.Effect.Window
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Query (allInvestigators)
import Arkham.Helpers.Xp
import Arkham.I18n
import Arkham.Id
import Arkham.Location.Grid
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Modifier (UIModifier (..))
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.TheThingInTheDepths.Helpers
import Arkham.Story.Cards qualified as Stories
import Arkham.Trait (Trait (Bog, Sunken))
import Data.Maybe (fromJust)

newtype TheThingInTheDepths = TheThingInTheDepths ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor TheThingInTheDepths where
  getModifiersFor (TheThingInTheDepths a) = do
    modifySelect a (assetIs Assets.drRosaMarquezBestInHerField) [DoNotTakeUpSlot #ally]

theThingInTheDepths :: Difficulty -> TheThingInTheDepths
theThingInTheDepths difficulty = scenario TheThingInTheDepths "10588" "The Thing in the Depths" difficulty []

instance HasChaosTokenValue TheThingInTheDepths where
  getChaosTokenValue iid tokenFace (TheThingInTheDepths attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage TheThingInTheDepths where
  runMessage msg s@(TheThingInTheDepths attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      day <- getCampaignDay
      time <- getCampaignTime
      let isNight = time == Night
      flavor do
        setTitle "title"
        p.basic "body"
        ul $ li.nested.validate isNight "nightSkip" do
          li.validate (not isNight && day == Day1) "day1"
          li.validate (not isNight && day == Day2) "day2"
          li.validate (not isNight && day == Day3) "day3"
      case (day, time) of
        (Day1, Day) -> do
          story $ i18nWithTitle "intro1"
          judithLevel <- getRelationshipLevel JudithPark
          let judithHighEnough = judithLevel >= 1
          flavor do
            p.basic "checkJudith"
            ul do
              li.validate judithHighEnough "judithHigh"
              li.validate (not judithHighEnough) "judithLow"
          if judithHighEnough
            then doStep 2 PreScenarioSetup
            else doStep 3 PreScenarioSetup
        (Day2, Day) -> doStep 4 PreScenarioSetup
        (Day3, Day) -> story $ i18nWithTitle "intro7"
        _ -> story $ i18nWithTitle "intro8"
      pure s
    DoStep 2 PreScenarioSetup -> scope "intro" do
      story $ i18nWithTitle "intro2"
      increaseRelationshipLevel JudithPark 1
      interludeXpAll (toBonus "bonus" 1)
      pure s
    DoStep 3 PreScenarioSetup -> scope "intro" do
      story $ i18nWithTitle "intro3"
      record JudithSharedAGrudge
      pure s
    DoStep 4 PreScenarioSetup -> scope "intro" do
      storyWithChooseOneM' (setTitle "intro4.title" >> p "intro4.body") do
        labeled' "push" $ doStep 5 PreScenarioSetup
        labeled' "rev" $ doStep 6 PreScenarioSetup
      pure s
    DoStep 5 PreScenarioSetup -> scope "intro" do
      story $ i18nWithTitle "intro5"
      eachInvestigator \iid -> setupModifier attrs iid (StartingResources (-2))
      pure s
    DoStep 6 PreScenarioSetup -> scope "intro" do
      story $ i18nWithTitle "intro6"
      eachInvestigator \iid -> setupModifier attrs iid (FewerActions 1)
      pure s
    Setup -> runScenarioSetup TheThingInTheDepths attrs do
      setup $ ul do
        li "gatherSets"
        li "gatherResidents"
        li "currentDaySet"
        li.nested "currentDayMarker" do
          li "doomDay2"
          li "doomDay3"
        li.nested "locations" do
          li "shuffleLocations"
          li "startingLocation"
        li.nested "residents" do
          li "judithDay1"
          li "drMarquez"
          li "riverDay2Day3"
          li "removeResidents"
        li "setAsideEnemies"
        unscoped $ li "shuffleRemainder"
        unscoped $ li "readyToBegin"

      scope "sinkingLocations" $ flavor $ setTitle "title" >> p "body"

      setUsesGrid

      gather Set.TheThingInTheDepths
      gather Set.Blight
      gather Set.TheForest
      gather Set.Mutations

      day <- getCampaignDay
      time <- getCampaignTime

      case day of
        Day1 -> do
          gather Set.TheFirstDay
          placeStory $ case time of
            Day -> Stories.dayOne
            Night -> Stories.nightOne
        Day2 -> do
          gather Set.TheSecondDay
          placeStory $ case time of
            Day -> Stories.dayTwo
            Night -> Stories.nightTwo
        Day3 -> do
          gather Set.TheFinalDay
          placeStory $ case time of
            Day -> Stories.dayThree
            Night -> Stories.nightThree

      setScenarioDayAndTime

      setAgendaDeck [Agendas.sinkingGround, Agendas.theThingInTheBog]
      setActDeck [Acts.aBotanicalSurvey, Acts.discoveryOfALifetime]

      case day of
        Day1 -> pure ()
        Day2 -> placeDoomOnAgenda 1
        Day3 -> placeDoomOnAgenda 2

      bogLocations <-
        drop 1
          <$> (shuffle =<< fromGathered (CardFromEncounterSet Set.TheThingInTheDepths <> #location))

      let
        gridPositions =
          [ Pos (-1) 1
          , Pos 0 1
          , Pos 1 1
          , Pos (-1) 0
          , Pos 0 0
          , Pos 1 0
          , Pos (-1) (-1)
          , Pos 0 (-1)
          , Pos 1 (-1)
          ]

      let
        posLabels :: Map Pos Scope =
          mapFromList
            [ (northShorePos, "ui.northShore")
            , (startingPos, "ui.startingLocation")
            , (cranberryBogPos, "ui.cranberryBog")
            ]

      placedLids <- for (zip gridPositions bogLocations) \(pos, card) -> do
        lid <- placeCardInGrid pos card
        for_ (lookup pos posLabels) \label ->
          gameModifier attrs lid (UIModifier $ ImportantToScenario $ ikey' label)
        pure (pos, lid)

      let
        lookupLid pos = snd $ fromJust $ find ((== pos) . fst) placedLids
        startingLid = lookupLid startingPos
        northShoreLid = lookupLid northShorePos

      startAt startingLid

      investigators <- allInvestigators

      when (time == Day) do
        when (day == Day1) do
          assetAt_ Assets.judithParkTheMuscle northShoreLid

          drMarquez <- createAsset =<< genCard Assets.drRosaMarquezBestInHerField
          leadChooseOneM do
            unscoped
              $ nameVar Assets.drRosaMarquezBestInHerField
              $ questionLabeled' "chooseInvestigatorToTakeControlOf"
            questionLabeledCard Assets.drRosaMarquezBestInHerField
            portraits investigators (`takeControlOfAsset` drMarquez)

        when (day == Day2 || day == Day3) do
          assetAt_ Assets.riverHawthorneBigInNewYork northShoreLid

      setAside
        [ Enemies.thingInTheDepths
        , Enemies.chelydranHybrid
        , Enemies.graspingTendril
        , Enemies.graspingTendril
        , Enemies.graspingTendril
        , Enemies.graspingTendril
        , Enemies.graspingTendril
        ]
    ScenarioSpecific "codex" v -> scope "codex" do
      let (iid :: InvestigatorId, source :: Source, n :: Int) = toResult v
      let entry x = scope x $ flavor $ setTitle "title" >> p.green "body"
      case n of
        5 -> do
          isControlled <- selectAny $ assetIs Assets.riverHawthorneBigInNewYork <> AssetControlledBy Anyone
          if isControlled
            then do
              scope "riverHawthorne" $ flavor $ setTitle "title" >> p.green "controlled"
              increaseRelationshipLevel RiverHawthorne 1
              interludeXpAll (toBonus "bonus" 1)
              selectForMaybeM (assetIs Assets.riverHawthorneBigInNewYork) removeFromGame
            else do
              codexFinished 5
              scope "riverHawthorne" $ flavor $ setTitle "title" >> p.green "uncontrolled"
              record RiverAskedForHelp
              investigators <- allInvestigators
              river <- selectJust $ assetIs Assets.riverHawthorneBigInNewYork
              leadChooseOneM do
                unscoped
                  $ nameVar Assets.riverHawthorneBigInNewYork
                  $ questionLabeled' "chooseInvestigatorToTakeControlOf"
                questionLabeledCard Assets.riverHawthorneBigInNewYork
                portraits investigators (`takeControlOfAsset` river)

                selectAny $ locationWithInvestigator iid <> LocationInPosition cranberryBogPos
              createAbilityEffect EffectGameWindow
                $ skillTestAbility
                $ onlyOnce
                $ restricted
                  (SourceableWithCardCode Assets.riverHawthorneBigInNewYork river)
                  1
                  ( OnSameLocation
                      <> youExist (InvestigatorAt (LocationInPosition cranberryBogPos))
                  )
                  parleyAction_
        7 -> do
          codexFinished 7
          judithShared <- getHasRecord JudithSharedAGrudge
          if judithShared
            then do
              scope "judithPark" $ flavor $ setTitle "title" >> p.green "grudge"
              enemies <- select $ NonEliteEnemy <> AnyInPlayEnemy
              chooseTargetM iid enemies \eid ->
                defeatEnemy eid iid source
            else do
              scope "judithPark" $ flavor $ setTitle "title" >> p.green "noGrudge"
          investigators <- allInvestigators
          judith <- selectJust $ assetIs Assets.judithParkTheMuscle
          leadChooseOneM do
            unscoped
              $ nameVar Assets.judithParkTheMuscle
              $ questionLabeled' "chooseInvestigatorToTakeControlOf"
            questionLabeledCard Assets.judithParkTheMuscle
            portraits investigators (`takeControlOfAsset` judith)
        Theta -> do
          codexFinished Theta
          entry "drRosaMarquez"
          locations <- select Anywhere
          chooseTargetM iid locations \lid -> do
            isBog <- lid <=~> LocationWithTrait Bog
            if isBog
              then removeAllOfTokenOn source #damage lid
              else do
                isSunken <- lid <=~> LocationWithTrait Sunken
                when isSunken $ flipOverBy iid source lid
        _ -> pure ()
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          resolution "noResolution"
          interludeXpAll (toBonus "bonus" 1)
          push R5
        Resolution 1 -> do
          record TheChelydranHybridPerished
          resolution "resolution1"
          interludeXpAll (toBonus "bonus" 1)
          push R5
        Resolution 2 -> do
          record TheChelydranHybridLived
          resolution "resolution2"
          interludeXpAll (toBonus "bonus" 3)
          push R5
        Resolution 3 -> do
          record TheChelydranHybridWasDevoured
          resolution "resolution3"
          eachInvestigator (`sufferMentalTrauma` 1)
          interludeXpAll (toBonus "bonus" 1)
          push R5
        Resolution 4 -> do
          record TheThingInTheDepthsWasDefeated
          resolution "resolution4"
          push R5
        Resolution 5 -> do
          riverAsked <- getHasRecord RiverAskedForHelp
          when riverAsked $ decreaseRelationshipLevel RiverHawthorne 1

          judithShared <- getHasRecord JudithSharedAGrudge
          thingInVictory <- selectAny $ VictoryDisplayCardMatch $ basic $ cardIs Enemies.thingInTheDepths
          judithBonus <-
            if judithShared && thingInVictory
              then do
                increaseRelationshipLevel JudithPark 1
                pure $ toBonus "bonusJudith" 1
              else pure NoBonus

          resolutionWithXp "resolution5" $ allGainXpWithBonus' attrs judithBonus
          record $ AreasSurveyed EastwickBog
          endOfScenario
        _ -> error "invalid resolution"
      pure s
    _ -> TheThingInTheDepths <$> liftRunMessage msg attrs
