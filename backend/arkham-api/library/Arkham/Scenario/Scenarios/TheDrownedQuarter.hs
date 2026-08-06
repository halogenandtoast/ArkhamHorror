module Arkham.Scenario.Scenarios.TheDrownedQuarter (theDrownedQuarter) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheDrownedCity.CampaignSteps (pattern TheApiary, pattern TheWesternWall)
import Arkham.Campaigns.TheDrownedCity.Import
import Arkham.Campaigns.TheDrownedCity.Key qualified as Key
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers (getFloodLevelFor)
import Arkham.Card
import Arkham.ChaosToken
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.Query
import Arkham.Helpers.Xp
import Arkham.Investigator.Projection ()
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.FloodLevel (FloodLevel (..))
import Arkham.Location.Grid (Pos (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Placement
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.TheDrownedQuarter.Helpers

newtype TheDrownedQuarter = TheDrownedQuarter ScenarioAttrs
  deriving stock Generic
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

theDrownedQuarter :: Difficulty -> TheDrownedQuarter
theDrownedQuarter difficulty = scenario TheDrownedQuarter "11536" "The Drowned Quarter" difficulty []

instance HasChaosTokenValue TheDrownedQuarter where
  getChaosTokenValue iid chaosTokenFace (TheDrownedQuarter attrs) = case chaosTokenFace of
    Skull -> do
      revealed <- selectCount RevealedLocation
      pure $ ChaosTokenValue Skull (NegativeModifier $ byDifficulty attrs (revealed `div` 2) revealed)
    Cultist -> pure $ toChaosTokenValue attrs Cultist 2 3
    Tablet -> pure $ toChaosTokenValue attrs Tablet 4 4
    ElderThing -> do
      flooded <- selectAny $ FloodedLocation <> locationWithInvestigator iid
      pure
        $ if flooded then toChaosTokenValue attrs ElderThing 3 4 else toChaosTokenValue attrs ElderThing 5 6
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage TheDrownedQuarter where
  runMessage msg s@(TheDrownedQuarter attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      headedWest <- getHasRecord TheExpeditionHeadedWest
      storyWithContinue' do
        setTitle "title"
        p "drownedQuarter1"
        p.basic "checkCampaignLog"
        ul do
          li.validate headedWest "headedWest"
          li.validate (not headedWest) "headedEast"

      investigators <- select (IncludeEliminated Anyone)
      withNoPlaceLikeHome <- filterM (`investigatorHasTask` Assets.noPlaceLikeHome) investigators
      flavor do
        setTitle "title"
        p $ if headedWest then "drownedQuarter2" else "drownedQuarter3"
        p $ if headedWest then "drownedQuarter2Conclusion" else "drownedQuarter3Conclusion"
        ul $ li.validate (notNull withNoPlaceLikeHome) "resolveNoPlaceLikeHome"
        p.basic "proceedToSetup"

      for_ withNoPlaceLikeHome \iid -> do
        hasPhysical <- fieldP InvestigatorPhysicalTrauma (> 0) iid
        hasMental <- fieldP InvestigatorMentalTrauma (> 0) iid
        canErase <- canEraseProgress iid Key.NoPlaceLikeHome
        storyWithChooseOneM'
          ( compose.green do
              h3 "noPlaceLikeHome.title"
              p "noPlaceLikeHome.instructions"
              p "noPlaceLikeHome.body"
              p "noPlaceLikeHome.question"
              p.basic "noPlaceLikeHome.choose"
              ul do
                li "noPlaceLikeHome.trustHim"
                li "noPlaceLikeHome.onMyOwn"
          )
          do
            labeledValidate' canErase "noPlaceLikeHome.trustHim" do
              -- "Heal 1 mental or 1 physical trauma"; only offer what they have.
              when (hasPhysical || hasMental) do
                chooseOneM iid do
                  questionLabeled' "noPlaceLikeHome.healTraumaQuestion"
                  when hasPhysical
                    $ labeled' "noPlaceLikeHome.healPhysicalTrauma"
                    $ push
                    $ HealTrauma iid 1 0
                  when hasMental
                    $ labeled' "noPlaceLikeHome.healMentalTrauma"
                    $ push
                    $ HealTrauma iid 0 1
              decrementRecordCountForInvestigator iid Key.NoPlaceLikeHome 1
            labeled' "noPlaceLikeHome.onMyOwn" do
              incrementRecordCountForInvestigator iid Key.NoPlaceLikeHome 2
              sufferMentalTrauma iid 1
              -- "The next scenario" is this one: the story is read in the intro,
              -- before setup, so the next scenario to begin is the one about to be
              -- set up. nextSetupModifier is inert while its own scenario is
              -- current and would silently do nothing here.
              for_ investigators \iid' -> setupModifier attrs iid' (StartingHand (-2))
      pure s
    StandaloneSetup -> do
      setChaosTokens (chaosBagContents attrs.difficulty)
      pure s
    Setup -> runScenarioSetup TheDrownedQuarter attrs do
      setUsesGrid
      headedWest <- getHasRecord TheExpeditionHeadedWest
      scope "setup" $ flavor do
        setTitle "title"
        ul do
          li "gatherSets"
          li.nested "locationPlacement" do
            li "placeBarrierCore"
            li "gatherSeaFloorLocations"
            li "placeSeaFloorLocations"
            li "beginAtBarrierCore"
          li "setCardsAside"
          li "chooseExpeditionAsset"
          li.nested "addFloodTokens" do
            li.validate (not headedWest) "headedEast"
          li "buildEncounterDeck"
          li "readyToBegin"

      gather Set.TheDrownedQuarter
      gather Set.AlienMachinery
      gather Set.CosmicLegacy
      gather Set.DeepOnes
      gather Set.ElderMist
      gather Set.Flood
      gather Set.Rlyeh
      gather Set.UnderseaCreatures

      setActDeck [Acts.reactivateTheCore]
      setAgendaDeck [Agendas.theSunkenRuins, Agendas.collapsingDome]

      setAside [Assets.barrierNode, Enemies.underseaParasite, Assets.obsidianRelic]

      -- Consume every gathered scenario location; the layout below creates exactly
      -- the copies that remain in this game.
      removeCards =<< amongGathered (CardFromEncounterSet Set.TheDrownedQuarter <> #location)

      acropolis <-
        sample2 Locations.drownedAcropolisEphemeralRuins Locations.drownedAcropolisCollapsedRuins
      coralReef <- sample2 Locations.coralReefStatuaryGarden Locations.coralReefFeedingGrounds
      seaFloor <-
        shuffleM
          [ Locations.abyssalTrench
          , Locations.abyssalTrench
          , Locations.abyssalTrench
          , acropolis
          , Locations.blastedRuinsSunkenCircle
          , Locations.blastedRuinsCrumblingEdifices
          , coralReef
          , Locations.ancientGallery
          ]

      -- The Barrier Core is the centre of a 3x3 grid and the eight shuffled Sea
      -- Floor locations fill the ring around it.
      barrierCore <- placeInGrid (Pos 0 0) Locations.barrierCoreInactive
      seaFloorIds <-
        for
          ( zip
              [ Pos (-1) 1
              , Pos 0 1
              , Pos 1 1
              , Pos (-1) 0
              , Pos 1 0
              , Pos (-1) (-1)
              , Pos 0 (-1)
              , Pos 1 (-1)
              ]
              seaFloor
          )
          $ uncurry placeInGrid
      startAt barrierCore

      unless headedWest do
        lead <- getLead
        n <- getPlayerCount
        chooseNM lead n do
          questionLabeled' "chooseFloodedSeaFloor"
          targets seaFloorIds increaseFloodLevel

      eachInvestigator (`forInvestigator` Setup)
    ForInvestigator iid Setup -> do
      artifacts <- getAvailableArtifacts
      chooseOneM iid do
        questionLabeled' "chooseExpeditionAssetQuestion"
        labeled' "noExpeditionAsset" nothing
        for_ (artifacts <> expeditionItems) \asset ->
          cardLabeled asset.cardCode $ handleTarget iid attrs (CardCodeTarget asset.cardCode)
      pure s
    HandleTargetChoice iid (isSource attrs -> True) (CardCodeTarget cardCode) -> do
      for_ (lookupCardDef cardCode) \def -> do
        card <- EncounterCard <$> genEncounterCard def
        createAssetAt_ card (InPlayArea iid)
      pure s
    ResolveChaosToken _ Cultist iid | isHardExpert attrs -> do
      whenM ((/= Unflooded) <$> getFloodLevelFor iid) $ assignDamage iid Cultist 1
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ n -> do
      let doPlaceClues amount = do
            clues <- iid.clues
            when (clues > 0) $ placeCluesOnLocation iid token.face (min amount clues)
      case token.face of
        Cultist | isEasyStandard attrs -> do
          whenM ((== FullyFlooded) <$> getFloodLevelFor iid) $ assignDamage iid Cultist 1
        Tablet -> doPlaceClues $ if isEasyStandard attrs then 1 else n
        _ -> pure ()
      pure s
    ScenarioResolution res -> scope "resolutions" do
      case res of
        NoResolution -> do
          resolution "noResolution"
          -- "Each investigator must erase 1 progress under their Task, if able."
          -- Everyone resigned or was defeated to get here, so include eliminated.
          investigators <- select (IncludeEliminated Anyone)
          for_ investigators \iid -> do
            investigatorTasks <- getInvestigatorTasks iid
            for_ investigatorTasks \(key, _, _) ->
              whenM ((> 0) <$> getRecordCountForInvestigator iid key)
                $ decrementRecordCountForInvestigator iid key 1
          push R3
        Resolution 1 -> do
          resolution "resolution1"
          record BarrierNode
          push R3
        Resolution 2 -> do
          resolution "resolution2"
          record ThePowerWasDiverted
          push R3
        Resolution 3 -> do
          -- Resolution 2 is the only route that grants the bonus, and it is the
          -- only place the power is recorded as diverted.
          powerWasDiverted <- getHasRecord ThePowerWasDiverted
          resolutionWithXp "resolution3"
            $ if powerWasDiverted
              then allGainXpWithBonus' attrs $ toBonus "bonus" 2
              else allGainXp' attrs
          crossOutRecordSetEntries RlyehMap [toJSON RlyehDrownedQuarter]
          headedWest <- getHasRecord TheExpeditionHeadedWest
          endOfScenarioThen $ if headedWest then TheApiary else TheWesternWall
        _ -> error $ "Unknown resolution: " <> show res
      pure s
    _ -> TheDrownedQuarter <$> liftRunMessage msg attrs
