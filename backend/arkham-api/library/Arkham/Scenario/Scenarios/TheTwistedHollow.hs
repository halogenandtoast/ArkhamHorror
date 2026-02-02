module Arkham.Scenario.Scenarios.TheTwistedHollow (theTwistedHollow) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheFeastOfHemlockVale.Key
import Arkham.EncounterSet qualified as Set
import Arkham.Helpers.Enemy (spawnAt)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Query (allInvestigators, getPlayerCount)
import Arkham.I18n
import Arkham.Id
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.TheTwistedHollow.Helpers
import Arkham.Spawn
import Arkham.Story.Cards qualified as Stories
import Arkham.Token

newtype TheTwistedHollow = TheTwistedHollow ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTwistedHollow :: Difficulty -> TheTwistedHollow
theTwistedHollow difficulty = scenario TheTwistedHollow "10605" "The Twisted Hollow" difficulty []

instance HasChaosTokenValue TheTwistedHollow where
  getChaosTokenValue iid tokenFace (TheTwistedHollow attrs) = case tokenFace of
    Skull -> do
      let darknessLevel = attrs.token DarknessLevel
      pure $ toChaosTokenValue attrs Skull darknessLevel (darknessLevel * 2)
    Cultist -> pure $ toChaosTokenValue attrs Cultist 1 3
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 4
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 4 5
    otherFace -> getChaosTokenValue iid otherFace attrs

tabletEffect :: ReverseQueue m => InvestigatorId -> m ()
tabletEffect iid = do
  withLocationOf iid \loc -> do
    validEnemies <- pursuitEnemiesWithHighestEvade
    chooseTargetM iid validEnemies \enemy -> spawnAt enemy Nothing (SpawnAtLocation loc)

instance RunMessage TheTwistedHollow where
  runMessage msg s@(TheTwistedHollow attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      storyWithChooseOneM' (setTitle "title" >> p "intro1") do
        labeled' "tellTheTruth" $ doStep 2 PreScenarioSetup
        labeled' "lie" $ doStep 3 PreScenarioSetup
      pure s
    DoStep 2 PreScenarioSetup -> scope "intro" do
      record MotherRachelShowedTheWay
      addChaosToken #cultist
      flavor $ setTitle "title" >> p "intro2"
      pure s
    DoStep 3 PreScenarioSetup -> scope "intro" do
      record TheInvestigatorsLostThePath
      addChaosToken #elderthing
      flavor $ setTitle "title" >> p "intro3"
      pure s
    Setup -> runScenarioSetup TheTwistedHollow attrs do
      setUsesGrid
      setup $ ul do
        li "gatherSets"
        li "night"
        li.nested "valeLantern.checkCampaignLog" do
          li "valeLantern.showedTheWay"
          li "valeLantern.lostThePath"
        li.nested "removeLocations" do
          li "oneOrTwoInvestigators"
          li "threeOrFourInvestigators"
        li.nested "glimmeringMeadow.checkCampaignLog" do
          li "glimmeringMeadow.showedTheWay"
          li "glimmeringMeadow.lostThePath"
        li "woodsDeck"
        li.nested "residents" do
          li "theoPeters"
          li "judithPark"
          li "drRosaMarquez"
          li "bertieMusgrave"
          li "rest"
        li "backToTheVale"
        unscoped $ li "shuffleRemainder"
        li "darknessLevel"
        unscoped $ li "readyToBegin"

      gather Set.TheFirstDay
      gather Set.TheTwistedHollow
      gather Set.TheForest
      gather Set.Myconids

      placeStory Stories.nightOne

      showedTheWay <- getHasRecord MotherRachelShowedTheWay

      let lanternVersion = if showedTheWay then Assets.valeLanternBeaconOfHope else Assets.valeLanternAFaintHope
      removeEvery [if showedTheWay then Assets.valeLanternAFaintHope else Assets.valeLanternBeaconOfHope]

      investigators <- allInvestigators
      lantern <- createAsset =<< fetchCard lanternVersion
      leadChooseOneM do
        unscoped
          $ nameVar lanternVersion
          $ questionLabeled' "chooseInvestigatorToTakeControlOf"
        questionLabeledCard lanternVersion
        portraits investigators (`takeControlOfAsset` lantern)

      setAside [Locations.theTwistedHollow, Locations.glimmeringMeadow]
      n <- getPlayerCount
      woods <-
        fmap (drop $ if n >= 3 then 1 else 2) . shuffle =<< fromGathered (CardWithTitle "Western Woods")

      glimmeringMeadow <- fromSetAside Locations.glimmeringMeadow

      if showedTheWay
        then do
          startAt =<< placeCardInGrid (Pos 0 0) glimmeringMeadow
          case woods of
            north : east : south : west : rest -> do
              for_
                [(Pos 0 (-1), north), (Pos 1 0, east), (Pos 0 1, south), (Pos (-1) 0, west)]
                (uncurry placeCardInGrid_)
              addExtraDeck WoodsDeck rest
            _ -> error "not enough woods"
        else do
          case woods of
            start : rest -> do
              startAt =<< placeCardInGrid (Pos 0 0) start
              shuffle (glimmeringMeadow : rest) >>= \case
                north : east : south : west : rest' -> do
                  for_
                    [(Pos 0 (-1), north), (Pos 1 0, east), (Pos 0 1, south), (Pos (-1) 0, west)]
                    (uncurry placeCardInGrid_)
                  addExtraDeck WoodsDeck rest'
                _ -> error "not enough woods"
            _ -> error "not enough woods"

      -- do this after locations so the reveal does not trigger
      setAgendaDeck [Agendas.deepeningDark]
      setActDeck [Acts.desperateSearch, Acts.wheresBertie]

      theo <- getRecordCount TheoPetersRelationshipLevel
      when (theo >= 2) $ setAside [Assets.theoPetersJackOfAllTrades]

      judith <- getRecordCount JudithParkRelationshipLevel
      when (judith >= 2) $ setAside [Assets.judithParkTheMuscle]

      drRosaMarquez <- createAsset =<< fetchCard Assets.drRosaMarquezBestInHerField
      leadChooseOneM do
        unscoped
          $ nameVar Assets.drRosaMarquezBestInHerField
          $ questionLabeled' "chooseInvestigatorToTakeControlOf"
        questionLabeledCard Assets.drRosaMarquezBestInHerField
        portraits investigators (`takeControlOfAsset` drRosaMarquez)

      setAside [Assets.bertieMusgraveATrueAesthete, Agendas.backToTheVale]
      placeTokens ScenarioSource ScenarioTarget DarknessLevel 1
    ResolveChaosToken _ Cultist iid | isEasyStandard attrs -> do
      healDamage iid Cultist 1
      pure s
    ResolveChaosToken _ Tablet iid | isHardExpert attrs -> do
      tabletEffect iid
      pure s
    ResolveChaosToken _ ElderThing iid | isHardExpert attrs -> do
      assignHorror iid ElderThing 1
      pure s
    PassedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        Cultist | isHardExpert attrs -> do
          healDamage iid Cultist 1
        _ -> pure ()
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        Tablet | isEasyStandard attrs -> tabletEffect iid
        ElderThing | isEasyStandard attrs -> assignHorror iid ElderThing 1
        _ -> pure ()
      pure s
    _ -> TheTwistedHollow <$> liftRunMessage msg attrs
