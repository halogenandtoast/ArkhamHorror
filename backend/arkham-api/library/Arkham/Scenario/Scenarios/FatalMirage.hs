module Arkham.Scenario.Scenarios.FatalMirage (fatalMirage) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign.Option
import Arkham.CampaignLog hiding (Resolute)
import Arkham.Campaigns.EdgeOfTheEarth.CampaignSteps (
  pattern CityOfTheElderThings,
  pattern ToTheForbiddenPeaks,
 )
import Arkham.Campaigns.EdgeOfTheEarth.CampaignSteps qualified as Step
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Campaigns.EdgeOfTheEarth.Key
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.FlavorText
import Arkham.Game.Base
import {-# SOURCE #-} Arkham.Game.Utils
import Arkham.Helpers.Agenda
import Arkham.Helpers.Campaign
import Arkham.Helpers.Game (withAlteredGame)
import Arkham.Helpers.Log hiding (crossOutRecordSetEntries, recordSetInsert)
import Arkham.Helpers.Query
import Arkham.Helpers.Text
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Message.Lifted.Move
import Arkham.Placement
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Types (Scenario (..), victoryDisplayL)
import Arkham.Scenarios.FatalMirage.Helpers
import Arkham.Trait (Trait (Eidolon, Otherworld, Resolute))

newtype FatalMirage = FatalMirage ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fatalMirage :: Difficulty -> FatalMirage
fatalMirage difficulty =
  scenario
    FatalMirage
    "08549"
    "Fatal Mirage"
    difficulty
    [ ".           ottomanFront       .                desertedStation  alaskanWilds"
    , "airfield    .                  baseCamp         .                infirmary"
    , ".           coastalWaters      prisonOfMemories riverviewTheatre ."
    , "moaiStatues deckOfTheTheodosia .                universityHalls  drKenslersOffice"
    , ".           hedgeMaze          standingStones   elderChamber     clutteredDormitory"
    , ".           dyersClassroom     .                theBlackStone    ."
    ]

instance HasChaosTokenValue FatalMirage where
  getChaosTokenValue iid tokenFace (FatalMirage attrs) = case tokenFace of
    Skull -> do
      n <- selectCount $ VictoryDisplayCardMatch $ basic #story
      m <- getCurrentAgendaStep
      pure $ toChaosTokenValue attrs Skull n (n + m)
    Cultist -> pure $ ChaosTokenValue Cultist (NegativeModifier 2)
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 4
    ElderThing -> pure $ toChaosTokenValue attrs Tablet 4 5
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage FatalMirage where
  runMessage msg s@(FatalMirage attrs) = runQueueT $ scenarioI18n $ case msg of
    StandaloneSetup -> do
      setChaosTokens $ chaosBagContents attrs.difficulty
      pure s
    PreScenarioSetup -> do
      isStandalone <- getIsStandalone
      if isStandalone
        then do
          lead <- getLead
          chooseOneM lead do
            questionLabeled
              "The investigators may choose which agenda to use (the higher the agenda number, the less time the investigators will have)."
            cardLabeled Agendas.etherealTangleV1 (doStep 1 msg)
            cardLabeled Agendas.etherealTangleV2 (doStep 2 msg)
            cardLabeled Agendas.etherealTangleV3 (doStep 3 msg)
        else doStep 0 msg
      pure s
    DoStep n PreScenarioSetup | n > 0 -> do
      doStep 0 PreScenarioSetup
      pure $ case n of
        1 -> overAttrs (standaloneCampaignLogL . optionsL %~ insertSet FatalMiragePart1) s
        2 -> overAttrs (standaloneCampaignLogL . optionsL %~ insertSet FatalMiragePart2) s
        3 -> overAttrs (standaloneCampaignLogL . optionsL %~ insertSet FatalMiragePart3) s
        _ -> s
    DoStep 0 PreScenarioSetup -> do
      isStandalone <- getIsStandalone
      when (not isStandalone || attrs.hasOption PerformIntro) do
        completed <- elem Step.FatalMirage <$> getCompletedSteps
        if not completed && not (attrs.hasOption FatalMiragePart2) && not (attrs.hasOption FatalMiragePart3)
          then do
            story $ i18nWithTitle "intro1"
            killedInThePlaneCrash <- getRecordSet WasKilledInThePlaneCrash
            when (recorded Assets.professorWilliamDyerProfessorOfGeology.cardCode `elem` killedInThePlaneCrash) do
              blueStory $ i18nEntry "dyerWasKilledInThePlaneCrash"

            when (recorded Assets.roaldEllsworthIntrepidExplorer.cardCode `elem` killedInThePlaneCrash) do
              blueStory $ i18nEntry "ellsworthWasKilledInThePlaneCrash"

            when (recorded Assets.eliyahAshevakDogHandler.cardCode `elem` killedInThePlaneCrash) do
              blueStory $ i18nEntry "ashevakWasKilledInThePlaneCrash"

            when (recorded Assets.danforthBrilliantStudent.cardCode `elem` killedInThePlaneCrash) do
              blueStory $ i18nEntry "danforthWasKilledInThePlaneCrash"

            when (recorded Assets.jamesCookieFredericksDubiousChoice.cardCode `elem` killedInThePlaneCrash) do
              blueStory $ i18nEntry "cookieWasKilledInThePlaneCrash"

            when (recorded Assets.averyClaypoolAntarcticGuide.cardCode `elem` killedInThePlaneCrash) do
              blueStory $ i18nEntry "claypoolWasKilledInThePlaneCrash"

            when (recorded Assets.takadaHirokoAeroplaneMechanic.cardCode `elem` killedInThePlaneCrash) do
              blueStory $ i18nEntry "takadaWasKilledInThePlaneCrash"

            when (recorded Assets.drMalaSinhaDaringPhysician.cardCode `elem` killedInThePlaneCrash) do
              blueStory $ i18nEntry "sinhaWasKilledInThePlaneCrash"

            when (recorded Assets.drAmyKenslerProfessorOfBiology.cardCode `elem` killedInThePlaneCrash) do
              blueStory $ i18nEntry "kenslerWasKilledInThePlaneCrash"
          else do
            story $ i18nWithTitle "intro2"

      eachInvestigator (`forInvestigator` PreScenarioSetup)
      pure s
    ForInvestigator iid PreScenarioSetup -> do
      partners <- getRemainingPartners
      unless (null partners) do
        chooseOneM iid do
          questionLabeled "Choose a partner for this scenario"
          labeled "Do not take a partner" nothing
          for_ partners \partner -> do
            inPlay <- selectAny $ assetIs partner.cardCode
            unless inPlay do
              cardLabeled partner.cardCode $ handleTarget iid ScenarioSource (CardCodeTarget partner.cardCode)
      pure s
    HandleTargetChoice iid (isSource attrs -> True) (CardCodeTarget cardCode) -> do
      for_ (lookupCardDef cardCode) \def -> do
        card <- genCard def
        assetId <- createAssetAt card (InPlayArea iid)
        partner <- getPartner cardCode
        pushWhen (partner.damage > 0) $ Msg.PlaceDamage CampaignSource (toTarget assetId) partner.damage
        pushWhen (partner.horror > 0) $ Msg.PlaceHorror CampaignSource (toTarget assetId) partner.horror
      pure s
    Setup -> runScenarioSetup FatalMirage attrs do
      gather Set.FatalMirage
      gather Set.AgentsOfTheUnknown
      gather Set.LeftBehind
      gather Set.Miasma
      gather Set.NamelessHorrors
      gather Set.SilenceAndMystery
      gather Set.Tekelili
      gather Set.ChillingCold

      completedSteps <- getCompletedSteps
      let fatalMirageTimes = count (== Step.FatalMirage) completedSteps + 1

      if
        | fatalMirageTimes == 3 || attrs.hasOption FatalMiragePart3 -> do
            setAgendaDeck [Agendas.etherealTangleV3]
            setActDeck [Acts.shadowOfThePastV3]
        | fatalMirageTimes == 2 || attrs.hasOption FatalMiragePart2 -> do
            setAgendaDeck [Agendas.etherealTangleV2]
            setActDeck [Acts.shadowOfThePastV2]
        | otherwise -> do
            setAgendaDeck [Agendas.etherealTangleV1]
            setActDeck [Acts.shadowOfThePastV1]

      memoriesBanished <- getRecordSet MemoriesBanished
      memoriesDiscovered <- getRecordSet MemoriesDiscovered

      startAt =<< place Locations.prisonOfMemories

      for_ (recordedCardCodes memoriesDiscovered) \cardCode -> do
        location <- amongGathered (CardWithCardCode cardCode)
        for_ location (place_ . toCardDef)

      setAside =<< amongGathered #location

      let
        (banished, enemies) =
          partition
            (maybe False ((`elem` memoriesBanished) . recorded) . cdOtherSide)
            [ Enemies.memoryOfAHuntGoneAwry
            , Enemies.memoryOfALostPatient
            , Enemies.memoryOfAMissingFather
            , Enemies.memoryOfARavagedCountry
            , Enemies.memoryOfARegretfulVoyage
            , Enemies.memoryOfAnUnspeakableEvil
            , Enemies.memoryOfATerribleDiscovery
            , Enemies.memoryOfAnAlienTranslation
            , Enemies.memoryOfAnUnrequitedLove
            ]
      setAside enemies
      placeInVictory $ mapMaybe (lookupCardDef <=< cdOtherSide) banished

      setAside =<< amongGathered (CardWithTrait Resolute)

      case attrs.difficulty of
        Expert -> placeDoomOnAgenda 2
        Hard -> placeDoomOnAgenda 1
        _ -> pure ()

      addTekeliliDeck
    PassedSkillTest iid _ _ (ChaosTokenTarget token) _ n -> do
      case token.face of
        Cultist | n < 2 -> placeCluesOnLocation iid Cultist 1
        _ -> pure ()
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ n -> do
      case token.face of
        Cultist -> placeCluesOnLocation iid Cultist 1
        Tablet -> do
          atPrison <- iid <=~> investigatorAt (locationIs Locations.prisonOfMemories)
          if isHardExpert attrs && n >= 2
            then do
              unless atPrison $ moveTo_ Tablet iid Locations.prisonOfMemories
              addTekelili iid . take 1 =<< getScenarioDeck TekeliliDeck
            else do
              chooseOneM iid do
                unless atPrison do
                  labeled "Move to the Prison of Memories" $ moveTo_ Tablet iid Locations.prisonOfMemories
                labeled "Shuffle the top card of the Tekeli-li deck into your deck without looking at it." do
                  addTekelili iid . take 1 =<< getScenarioDeck TekeliliDeck
        ElderThing ->
          chooseSelectM iid (EnemyWithTrait Eidolon) \enemy -> placeDoom ElderThing enemy 1
        _ -> pure ()
      pure s
    ScenarioResolution r -> scope "resolutions" do
      completed <- getCompletedSteps
      let step3 = CityOfTheElderThings `elem` completed
      let step2 = not step3 && ToTheForbiddenPeaks `elem` completed
      let step1 = not step3 && not step2
      story
        $ setFlavorTitle (if r == NoResolution then "No Resolution" else "Resolution 1")
        $ flavorText
        $ modifyEntry ResolutionEntry
        $ compose
          [ p $ if r == NoResolution then "noResolution" else "resolution1"
          , ul do
              li $ if r == NoResolution then "noSay" else "isThereADifference"
              li "victory"
              li.nested "memoriesDiscovered" do
                li "crossOutMemoriesDiscovered"
              li "nonStoryVictory"
              li "partnerDamage"
              li.nested "proceed" do
                li.validate step1 "proceedToTheForbiddenPeaks"
                li.validate step2 "proceedToCityOfTheElderThings"
                li.validate step3 "proceedToTheHeartOfMadness"
          ]
      memoriesInVictory <- select (VictoryDisplayCardMatch #story)
      previouslyBanished <- getRecordSet MemoriesBanished
      let newMemories = map toCardDef $ filter ((`notElem` previouslyBanished) . recorded . toCardCode) memoriesInVictory
      -- we want to remove all memories before calculating
      for_ memoriesInVictory obtainCard
      recordSetInsert MemoriesBanished $ map toCardCode memoriesInVictory

      discovered <- selectWithField LocationCardCode $ not_ (LocationWithTrait Otherworld)
      recordSetInsert MemoriesDiscovered $ map snd discovered

      crossOutRecordSetEntries MemoriesDiscovered
        $ [Locations.alaskanWilds.cardCode | Enemies.memoryOfAHuntGoneAwry `elem` newMemories]
        <> [Locations.infirmaryFatalMirage.cardCode | Enemies.memoryOfALostPatient `elem` newMemories]
        <> [Locations.airfield.cardCode | Enemies.memoryOfAMissingFather `elem` newMemories]
        <> [Locations.ottomanFront.cardCode | Enemies.memoryOfARavagedCountry `elem` newMemories]
        <> [Locations.dyersClassroom.cardCode | Enemies.memoryOfARegretfulVoyage `elem` newMemories]
        <> [Locations.clutteredDormitory.cardCode | Enemies.memoryOfAnUnspeakableEvil `elem` newMemories]
        <> [Locations.theBlackStone.cardCode | Enemies.memoryOfATerribleDiscovery `elem` newMemories]
        <> [Locations.moaiStatues.cardCode | Enemies.memoryOfAnAlienTranslation `elem` newMemories]
        <> [Locations.drKenslersOffice.cardCode | Enemies.memoryOfAnUnrequitedLove `elem` newMemories]

      withAlteredGame
        ( modeL
            %~ setScenario
              ( Scenario
                  $ FatalMirage
                  $ attrs
                  & victoryDisplayL
                  %~ filter ((`notElem` previouslyBanished) . recorded . toCardCode)
              )
        )
        (allGainXp attrs)
      endOfScenario
      pure s
    HandleOption option -> do
      whenM getIsStandalone $ do
        case option of
          PerformIntro -> pure ()
          _ -> error $ "Unhandled option: " <> show option
      pure s
    _ -> FatalMirage <$> liftRunMessage msg attrs
