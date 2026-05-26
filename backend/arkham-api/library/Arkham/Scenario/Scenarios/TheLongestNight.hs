module Arkham.Scenario.Scenarios.TheLongestNight (theLongestNight) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Campaigns.TheFeastOfHemlockVale.Key
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Query (allInvestigators, getLead)
import Arkham.I18n
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid
import Arkham.Matcher hiding (enemyAt)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Message.Lifted.Move
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TheLongestNight.Helpers
import Arkham.Story.Cards qualified as Stories
import Arkham.Token
import Arkham.Treachery.Cards qualified as Treacheries

newtype TheLongestNight = TheLongestNight ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor TheLongestNight where
  getModifiersFor (TheLongestNight a) = do
    modifySelect a (assetIs Assets.drRosaMarquezBestInHerField) [DoNotTakeUpSlot #ally]
    modifySelect a (assetIs Assets.helenPetersTheEldestSister) [DoNotTakeUpSlot #ally]

theLongestNight :: Difficulty -> TheLongestNight
theLongestNight difficulty = scenario TheLongestNight "10626" "The Longest Night" difficulty []

instance HasChaosTokenValue TheLongestNight where
  getChaosTokenValue iid tokenFace (TheLongestNight attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage TheLongestNight where
  runMessage msg s@(TheLongestNight attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      storyWithChooseOneM' (setTitle "title" >> p "intro1.body") do
        labeled' "confront" $ doStep 2 PreScenarioSetup
        labeled' "keepHidden" $ doStep 3 PreScenarioSetup
      pure s
    DoStep 2 PreScenarioSetup -> scope "intro" do
      story $ i18nWithTitle "intro2"
      addChaosToken #tablet
      doStep 4 PreScenarioSetup
      pure s
    DoStep 3 PreScenarioSetup -> scope "intro" do
      story $ i18nWithTitle "intro3"
      addChaosToken #skull
      doStep 4 PreScenarioSetup
      pure s
    DoStep 4 PreScenarioSetup -> scope "intro" do
      danceCount <-
        length
          <$> filterM
            getHasRecord
            [ toCampaignLogKey SimeonSharedADance
            , toCampaignLogKey LeahSharedADance
            , toCampaignLogKey TheoSharedADance
            , toCampaignLogKey GideonSharedADance
            , toCampaignLogKey JudithSharedADance
            , toCampaignLogKey WilliamSharedADance
            , toCampaignLogKey RiverSharedADance
            , toCampaignLogKey HelenSharedADance
            ]
      story $ i18nWithTitle "intro4"
      flavor do
        p.basic "checkDance"
        ul do
          li.validate (danceCount >= 2) "danceTwo"
          li.validate (danceCount == 1) "danceOne"
          li.validate (danceCount == 0) "danceNone"
      if danceCount >= 2
        then doStep 5 PreScenarioSetup
        else
          if danceCount == 1
            then doStep 6 PreScenarioSetup
            else doStep 7 PreScenarioSetup
      pure s
    DoStep 5 PreScenarioSetup -> scope "intro" do
      story $ i18nWithTitle "intro5"
      addChaosToken #cultist
      doStep 8 PreScenarioSetup
      pure s
    DoStep 6 PreScenarioSetup -> scope "intro" do
      story $ i18nWithTitle "intro6"
      addChaosToken #cultist
      addChaosToken #elderthing
      doStep 8 PreScenarioSetup
      pure s
    DoStep 7 PreScenarioSetup -> scope "intro" do
      story $ i18nWithTitle "intro7"
      record TheInvestigatorsFacedTheLongestNightAlone
      addChaosToken #elderthing
      doStep 8 PreScenarioSetup
      pure s
    DoStep 8 PreScenarioSetup -> scope "intro" do
      story $ i18nWithTitle "intro8"
      pure s
    Setup -> runScenarioSetup TheLongestNight attrs do
      setup $ ul do
        li "gatherSets"
        li "midnightMasks"
        li "nightTwo"
        li "removeOuterFields"
        li.nested "locations" do
          li "placeLocations"
          li "theCaptives"
          li "startingLocation"
        li.nested "enemyDeck" do
          li "gatherEnemies"
          li "ursineHybrid"
          li "shuffleEnemyDeck"
        li "bearWounded"
        li.nested "defenses" do
          li "placeDefenses"
          li "tokenReferenceCard"
        li.nested "clues" do
          li "facedAlone"
          li "facedWithHelp"
        li.nested "residents" do
          li "drMarquez"
          li "dancedResidents"
          li "helenPeters"
          li "removeResidents"
        li "setAside"
        unscoped $ li "shuffleRemainder"
        unscoped $ li "readyToBegin"

      scope "barriers" $ flavor $ setTitle "title" >> p "body"
      scope "decoys" $ flavor $ setTitle "title" >> p "body"
      scope "traps" $ flavor $ setTitle "title" >> p "body"
      scope "enemyDeckRules" $ flavor $ setTitle "title" >> p "body"

      setUsesGrid

      gather Set.TheSecondDay
      gather Set.TheLongestNight
      gather Set.Blight
      gather Set.Transfiguration
      gather Set.Fire
      gather Set.ChillingCold
      gatherJust Set.TheMidnightMasks [Treacheries.huntingShadow, Treacheries.falseLead]
      gather Set.StrikingFear

      placeStory Stories.nightTwo
      setScenarioDayAndTime

      setAgendaDeck [Agendas.theOnslaught]
      setActDeck [Acts.theLongestNight]

      (removed, remainingFields) <-
        splitAt 1
          <$> shuffle
            [ Locations.outerFieldsBloodiedPaths
            , Locations.outerFieldsDesolateHills
            , Locations.outerFieldsBlightedCornfields
            , Locations.outerFieldsScorchedKnoll
            , Locations.outerFieldsRancidCrops
            ]

      removeEvery removed

      farmhouse <- placeInGrid (Pos 0 0) Locations.theFarmhouse

      atwoodFarms <-
        shuffle [Locations.milkhouse, Locations.vineyard, Locations.coop, Locations.barn, Locations.pasture]
      let farmPositions = [Pos (-1) 0, Pos (-2) 0, Pos 1 0, Pos 0 (-1), Pos 0 1]
      farmLids <- for (zip farmPositions atwoodFarms) (uncurry placeInGrid)

      let westOuterPos = Pos (-3) 0
      let outerPositions = [westOuterPos, Pos 2 0, Pos 0 (-2), Pos 0 2]
      outerFields <- for (zip outerPositions remainingFields) \(pos, card) ->
        (pos,) <$> placeInGrid pos card
      let outerLids = map snd outerFields
      let allLocations = farmhouse : farmLids <> outerLids

      assetAt_ Assets.theCaptives farmhouse

      eachInvestigator \iid -> chooseTargetM iid allLocations $ moveTo_ attrs iid

      let westOuterFieldsLid = maybe farmhouse snd $ find ((== westOuterPos) . fst) outerFields
      ursine <- enemyAt Enemies.ursineHybrid westOuterFieldsLid

      enemyCards <- fromGathered (CardFromEncounterSet Set.TheLongestNight <> #enemy)
      addExtraDeck EnemyDeck =<< shuffle enemyCards

      bearWounded <- getHasRecord TheBearWasWounded
      when bearWounded do
        nonAttackEnemyDamage Nothing ScenarioSource 2 ursine
        exhaustEnemy ScenarioSource ursine

      lead <- getLead
      chooseTargetM lead allLocations \lid ->
        placeTokens ScenarioSource lid Horror 1
      chooseTargetM lead allLocations \lid ->
        placeTokens ScenarioSource lid Damage 1
      chooseTargetM lead allLocations (`forTarget` Setup)

      facedAlone <- getHasRecord TheInvestigatorsFacedTheLongestNightAlone
      let clueCount = if facedAlone then 3 else 2
      eachInvestigator \iid -> gainClues iid ScenarioSource clueCount

      investigators <- allInvestigators

      drMarquez <- createAsset =<< genCard Assets.drRosaMarquezBestInHerField
      leadChooseOneM do
        unscoped
          $ nameVar Assets.drRosaMarquezBestInHerField
          $ questionLabeled' "chooseInvestigatorToTakeControlOf"
        questionLabeledCard Assets.drRosaMarquezBestInHerField
        portraits investigators (`takeControlOfAsset` drMarquez)

      let danceChecks =
            [ (toCampaignLogKey SimeonSharedADance, SimeonAtwood)
            , (toCampaignLogKey LeahSharedADance, LeahAtwood)
            , (toCampaignLogKey TheoSharedADance, TheoPeters)
            , (toCampaignLogKey GideonSharedADance, GideonMizrah)
            , (toCampaignLogKey JudithSharedADance, JudithPark)
            , (toCampaignLogKey WilliamSharedADance, WilliamHemlock)
            , (toCampaignLogKey RiverSharedADance, RiverHawthorne)
            ]
      dancedResidents <-
        catMaybes <$> for danceChecks \(k, resident) -> do
          danced <- getHasRecord k
          pure $ if danced then Just resident else Nothing

      for_ dancedResidents \resident -> do
        residentAsset <- createAsset =<< genCard (toCardDef resident)
        leadChooseOneM do
          unscoped
            $ nameVar (toCardDef resident)
            $ questionLabeled' "chooseInvestigatorToTakeControlOf"
          questionLabeledCard (toCardDef resident)
          portraits investigators (`takeControlOfAsset` residentAsset)

      helenDanced <- getHasRecord HelenSharedADance
      when helenDanced do
        helenPeters <- createAsset =<< genCard Assets.helenPetersTheEldestSister
        leadChooseOneM do
          unscoped
            $ nameVar Assets.helenPetersTheEldestSister
            $ questionLabeled' "chooseInvestigatorToTakeControlOf"
          questionLabeledCard Assets.helenPetersTheEldestSister
          portraits investigators (`takeControlOfAsset` helenPeters)

      setAsideEvery (cardIs Treacheries.fire)
      setAside [Assets.ajax]
    ForTarget (LocationTarget lid) Setup -> do
      lead <- getLead
      connected <- select $ connectedTo (LocationWithId lid)
      chooseTargetM lead connected \lid2 ->
        push $ ScenarioCountIncrementBy (Barriers lid lid2) 1
      pure s
    _ -> TheLongestNight <$> liftRunMessage msg attrs
