module Arkham.Scenario.Scenarios.WrittenInRock (writtenInRock) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Direction
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Creation (createExhausted)
import Arkham.Helpers.Act
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement as Place
import Arkham.Projection
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.WrittenInRock.Helpers
import Arkham.Story.Cards qualified as Stories
import Arkham.Token
import Arkham.Trait (Trait (Cave, Rail))

newtype WrittenInRock = WrittenInRock ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

writtenInRock :: Difficulty -> WrittenInRock
writtenInRock difficulty =
  scenarioWith WrittenInRock "10501" "Written in Rock" difficulty []
    $ referenceL
    .~ if difficulty `elem` [Easy, Standard] then "10501" else "10502"

instance HasModifiersFor WrittenInRock where
  getModifiersFor (WrittenInRock a) = do
    n <- getCurrentActStep
    when (n == 2) do
      selectEach Anywhere \locA -> do
        pos <- fieldJust LocationPosition locA
        whenMatch locA (LocationWithoutModifier CannotBeSlidOrSwapped) do
          unlessM (null <$> getEmptyPositions locA) $ modified_ a locA [CanBeSlid]
        defA <- field LocationCardDef locA
        for_ (lookup "rails" (cdMeta defA)) \railsA -> do
          for_ (toResultDefault [] railsA) \dir -> do
            selectEach (LocationInPosition $ updatePosition pos dir) \locB -> do
              defB <- field LocationCardDef locB
              for_ (lookup "rails" (cdMeta defB)) \rails -> do
                when (oppositeDirection dir `elem` toResultDefault [] rails) do
                  modified_ a locA [ConnectedToWhen (LocationWithId locA) (LocationWithId locB)]

instance HasChaosTokenValue WrittenInRock where
  getChaosTokenValue iid tokenFace (WrittenInRock attrs) = case tokenFace of
    Skull -> do
      n <- runDefaultMaybeT 0 do
        loc <- MaybeT $ getLocationOf iid
        pos <- MaybeT $ field LocationPosition loc
        pure pos.column
      pure $ toChaosTokenValue attrs Skull n (n * 2)
    Cultist -> do
      n <- getCurrentActStep
      pure $ toChaosTokenValue attrs Cultist (if n == 1 then 1 else 2) 3
    Tablet -> do
      n <- getCurrentActStep
      pure $ toChaosTokenValue attrs Tablet (if n == 1 then 2 else 3) 4
    ElderThing -> do
      n <- getCurrentActStep
      pure $ toChaosTokenValue attrs ElderThing (if n == 1 then 3 else 4) 5
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage WrittenInRock where
  runMessage msg s@(WrittenInRock attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      story $ i18nWithTitle "intro"
      pure s
    Setup -> runScenarioSetup WrittenInRock attrs do
      setup $ ul do
        li "gatherSets"
        li "currentDaySet"
        li "currentDayMarker"
        li "caves"
        li "otherLocations"
        li.nested "scrap" do
          li "startAt"
        li.nested "residents" do
          li "riverHawthorne"
          li "simeonAtwood"
          li "leahAtwood"
          li "remainingResidents"
        li "subterraneanBeast"
        li "scenarioReference"
        li "setOutOfPlay"
        unscoped $ li "shuffleRemainder"
        unscoped $ li "readyToBegin"

      setUsesGrid
      gather Set.WrittenInRock
      gather Set.HorrorsInTheRock
      gather Set.Refractions
      gather Set.ChillingCold
      gather Set.Ghouls

      setAgendaDeck [Agendas.undergroundSurvey, Agendas.dangerousRide]
      setActDeck [Acts.descentIntoTheMines, Acts.theUndergroundMaze]
      setScenarioDayAndTime

      day <- getCampaignDay
      time <- getCampaignTime

      case day of
        Day1 -> do
          gather Set.TheFirstDay
          placeStory $ case time of
            Day -> Stories.dayOne
            Night -> Stories.nightOne
          setAside [Assets.simeonAtwoodDedicatedTroublemaker]
        Day2 -> do
          gather Set.TheSecondDay
          placeStory $ case time of
            Day -> Stories.dayTwo
            Night -> Stories.nightTwo
          setAside [Assets.simeonAtwoodDedicatedTroublemaker]
        Day3 -> do
          gather Set.TheFinalDay
          placeStory $ case time of
            Day -> Stories.dayThree
            Night -> Stories.nightThree
          setAside [Assets.leahAtwoodTheValeCook]

      controlStation <- placeInGrid (Pos 5 1) Locations.controlStation
      placeTokens ScenarioSource controlStation Scrap 1

      caves <- sampleListN 4 =<< fromGathered (#location <> withTrait Cave)
      setAside =<< fromGathered #location

      -- reverse so all caves are places when start at is triggered
      for_ (reverse $ withIndex1 caves) \(x, cave) -> do
        loc <- placeCardInGrid (Pos x 1) cave
        placeTokens ScenarioSource loc Scrap 1
        when (x == 1) $ startAt loc
        when (x == 3 && day == Day1) $ assetAt_ Assets.riverHawthorneBigInNewYork loc

      when (time == Day) $ removeEvery [Enemies.subterraneanBeast]
      setAside =<< fromGathered (CardFromEncounterSet Set.WrittenInRock)
      setAside =<< fromGathered (cardIs Enemies.crystalParasite)
    ResolveChaosToken _ Cultist _iid | isEasyStandard attrs -> do
      n <- getCurrentActStep
      placeTokens Cultist attrs (if n == 1 then Scrap else Switch) 1
      pure s
    ResolveChaosToken _ Tablet _iid | isHardExpert attrs -> do
      n <- getCurrentActStep
      when (n == 1) $ removeTokens Tablet attrs Scrap 1
      -- TODO move mine cart
      pure s
    ResolveChaosToken _ ElderThing iid | isHardExpert attrs -> do
      n <- getCurrentActStep
      if n == 1
        then assignDamage iid ElderThing 1
        else removeTokens ElderThing attrs Switch 1
      pure s
    PassedSkillTest _iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        Cultist | isHardExpert attrs -> do
          n <- getCurrentActStep
          placeTokens Cultist attrs (if n == 1 then Scrap else Switch) 1
        _ -> pure ()
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        Tablet | isEasyStandard attrs -> do
          n <- getCurrentActStep
          when (n == 1) $ removeTokens Tablet attrs Scrap 1
        -- TODO Move mine cart
        ElderThing | isEasyStandard attrs -> do
          n <- getCurrentActStep
          if n == 1
            then assignDamage iid ElderThing 1
            else removeTokens ElderThing attrs Switch 1
        _ -> pure ()
      pure s
    ScenarioSpecific "theCaveIn" _ -> do
      let scrap = attrs.token Scrap
      isDay <- (== Day) <$> getCampaignTime
      flavor do
        setTitle "theCaveIn"
        p "theCaveIn1"
        ul do
          li.validate isDay "proceedToTheCaveIn2"
          li.validate (not isDay) "skipToTheCaveIn3"

      flavor $ setTitle "theCaveIn" >> p (if isDay then "theCaveIn2" else "theCaveIn3")
      flavor $ setTitle "theCaveIn" >> p "theCaveIn4"
      controlStation <- selectJust $ locationIs Locations.controlStation
      push $ PlaceGrid (GridLocation (Pos 1 1) controlStation)
      placeLocationInGrid_ (Pos 5 5) =<< fetchCard Locations.railExit
      topOfColumn2 <- placeLocationInGrid (Pos 2 4) =<< fetchCard Locations.sunkenRailA
      atwoods <-
        getSetAsideCardsMatching
          (mapOneOf cardIs [Assets.simeonAtwoodDedicatedTroublemaker, Assets.leahAtwoodTheValeCook])
      for_ atwoods (`createAssetAt_` AtLocation topOfColumn2)

      forkedRails <- getSetAsideCardsMatching (cardIs Locations.forkedRail)
      for_ (zip [Pos 2 2, Pos 3 3] forkedRails) (uncurry placeLocationInGrid_)

      placeLocationInGrid_ (Pos 4 2)
        =<< fetchCard
        =<< sample2 Locations.alkalineRailA Locations.warpedRailA
      placeLocationInGrid_ (Pos 4 4)
        =<< fetchCard
        =<< sample2 Locations.rightTurnA Locations.rightTurnB
      bottomOfColumn5 <-
        placeLocationInGrid (Pos 5 1)
          =<< fetchCard
          =<< sample2 Locations.leftTurnA Locations.leftTurnB

      createAssetAt_ Assets.prismaticShardAlienMeteorite $ AtLocation bottomOfColumn5

      placeLocationInGrid_ (Pos 5 3)
        =<< fetchCard
        =<< sample2 Locations.railBridge Locations.alkalineRailB

      unless isDay do
        createSetAsideEnemyWith_ Enemies.subterraneanBeast (AtLocation controlStation) createExhausted

      doStep 2 msg -- ensure set aside cards are updated
      mineCart <- createAssetAt Assets.mineCartReliableButBroken (AtLocation controlStation)
      eachInvestigator (`Place.place` InVehicle mineCart)
      leadChooseOneM do
        questionLabeled' "mineCart.facing"
        labeled' "mineCart.faceNorth" $ scenarioSpecific "rotate" North
        labeled' "mineCart.faceEast" $ scenarioSpecific "rotate" East
        labeled' "mineCart.faceSouth" $ scenarioSpecific "rotate" South
        labeled' "mineCart.faceWest" $ scenarioSpecific "rotate" West

      doStep 3 msg -- ensure set aside cards are updated
      pure
        $ WrittenInRock
        $ attrs
        & (tokensL %~ addTokens Switch (scrap + 1) . removeAllTokens Scrap)
        & (referenceL %~ flippedCardCode)
        & (gridL %~ deleteInGrid controlStation)
    DoStep 2 (ScenarioSpecific "theCaveIn" _) -> do
      rails <- getSetAsideCardsMatching (CardWithTrait Rail)
      let positions = [Pos 1 2, Pos 1 3, Pos 1 4, Pos 2 1, Pos 3 4, Pos 4 1, Pos 5 2]
      for_ (zip positions rails) (uncurry placeLocationInGrid_)
      pure s
    DoStep 3 (ScenarioSpecific "theCaveIn" _) -> do
      shuffleSetAsideEncounterSet Set.WrittenInRock
      shuffleEncounterDiscardBackIn
      pure s
    _ -> WrittenInRock <$> liftRunMessage msg attrs
