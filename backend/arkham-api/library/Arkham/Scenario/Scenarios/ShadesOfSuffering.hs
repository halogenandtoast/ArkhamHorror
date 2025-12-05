module Arkham.Scenario.Scenarios.ShadesOfSuffering (shadesOfSuffering) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Campaigns.TheScarletKeys.Key
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Keys
import Arkham.Campaigns.TheScarletKeys.Key.Matcher
import Arkham.Campaigns.TheScarletKeys.Key.Types
import Arkham.Campaigns.TheScarletKeys.Meta
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Campaign (withOwner)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Log
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Query (allInvestigators, getLead)
import Arkham.Helpers.Xp (toBonus)
import Arkham.I18n
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Placement
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.ShadesOfSuffering.Helpers
import Arkham.Token (countTokens)
import Arkham.Trait (Trait (Geist))

newtype ShadesOfSuffering = ShadesOfSuffering ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shadesOfSuffering :: Difficulty -> ShadesOfSuffering
shadesOfSuffering difficulty =
  scenario
    ShadesOfSuffering
    "09660"
    "Shades of Suffering"
    difficulty
    [ "circle .        squiggle ."
    , ".      triangle diamond   moon"
    , "square .        star     ."
    ]

instance HasChaosTokenValue ShadesOfSuffering where
  getChaosTokenValue iid tokenFace (ShadesOfSuffering attrs) = case tokenFace of
    Skull -> do
      theShadeReaper <- selectJust $ scarletKeyIs Keys.theShadeReaper
      chargeCount <- fieldMap ScarletKeyTokens (countTokens #charge) theShadeReaper
      let n = (chargeCount + 1) `div` 2
      pure $ toChaosTokenValue attrs Skull (min n 6) n
    Cultist -> pure $ toChaosTokenValue attrs Cultist 4 5
    Tablet -> pure $ toChaosTokenValue attrs Tablet 1 3
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 4 6
    otherFace -> getChaosTokenValue iid otherFace attrs

instance HasModifiersFor ShadesOfSuffering where
  getModifiersFor (ShadesOfSuffering a) = do
    flintRejoinedTheCell <- getHasRecord FlintRejoinedTheCell
    when flintRejoinedTheCell do
      modifySelect a (assetIs Assets.inspectorFlintWithPrideAndCare) [DoNotTakeUpSlot #ally]

instance RunMessage ShadesOfSuffering where
  runMessage msg s@(ShadesOfSuffering attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      flintTraveledToKualaLumpur <- getHasRecord FlintTraveledToKualaLumpur
      flavor do
        setTitle "title"
        p "body"
        ul do
          li.validate flintTraveledToKualaLumpur "flintTraveledToKualaLumpur"
          li.validate (not flintTraveledToKualaLumpur) "agentFlintIsMissing"
      doStep (if flintTraveledToKualaLumpur then 1 else 2) msg
      setupKeys
      pure s
    DoStep 1 PreScenarioSetup -> scope "intro" do
      record FlintRejoinedTheCell
      flavor $ setTitle "title" >> p "intro1"
      iids <- allInvestigators
      addCampaignCardToDeckChoice iids ShuffleIn Assets.inspectorFlintWithPrideAndCare
      pure s
    DoStep 2 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "intro1"
      pure s
    Setup -> runScenarioSetup ShadesOfSuffering attrs do
      t <- getTime
      setup $ ul do
        li "gatherSets"
        li.nested "placeLocations" do
          li "setAsideOtherLocations"
        li.nested "checkCampaignLog" do
          li "flintRejoinedTheCell"
          li "agentFlintIsMissing"
        li "geists"
        li "tzuSanNiang"
        li.nested "time" do
          li.validate (t <= 11) "elevenOrFewerTime"
          li.validate (t >= 12 && t <= 18) "betweenTwelveAndEighteenTime"
          li.validate (t >= 19 && t <= 26) "betweenNineteenAndTwentySixTime"
          li.validate (t >= 27) "twentySevenOrMoreTime"
        unscoped $ li "shuffleRemainder"
        unscoped $ li "readyToBegin"
      gather Set.ShadesOfSuffering
      gather Set.DarkVeiling
      gather Set.MysteriesAbound
      gather Set.ScarletSorcery
      gather Set.SpreadingCorruption
      gather Set.StrikingFear
      handleRedCoterie

      setAgendaDeck [Agendas.painfulHistory, Agendas.restlessDead, Agendas.fearTheReaper]
      setActDeck [Acts.theLadyWithTheRedParasol, Acts.ghostLight, Acts.harvesterOfWoe]

      kualaLumpurStationWestWing <- place Locations.kualaLumpurStationWestWing
      selangorClub <- place Locations.selangorClub

      placeAll [Locations.kualaLumpurStationEastWing, Locations.selangorClubPadang]
      setAside [Locations.melatisShop, Locations.tinMine, Locations.wayangKulitTheater]

      flintRejoinedTheCell <- getHasRecord FlintRejoinedTheCell
      startAt $ if flintRejoinedTheCell then selangorClub else kualaLumpurStationWestWing
      when flintRejoinedTheCell do
        withOwner Assets.inspectorFlintWithPrideAndCare \iid -> do
          flint <- fetchCard Assets.inspectorFlintWithPrideAndCare
          chooseOneM iid $ unscoped do
            nameVar flint $ labeled' "putIntoPlay" $ putCardIntoPlay iid flint
            skip_

      uncannyShadow <-
        pickFrom (Enemies.uncannyShadowPlayfulShadows, Enemies.uncannyShadowTimorousShadows)
      buriedMiner <- pickFrom (Enemies.buriedMinerALostMemento, Enemies.buriedMinerExhumeTheBones)
      slainForeman <- pickFrom (Enemies.slainForemanSympathyPain, Enemies.slainForemanSympathyPain)
      setAside [uncannyShadow, buriedMiner, slainForeman]

      lead <- getLead
      tzuSanNiang <- fetchCard Enemies.tzuSanNiangTheLadyWithTheRedParasol
      drawCard lead tzuSanNiang
      doStep 1 Setup
    DoStep 1 Setup -> do
      tzuSanNiang <- selectJust $ enemyIs Enemies.tzuSanNiangTheLadyWithTheRedParasol
      theShadeReaper <- createScarletKeyAt Keys.theShadeReaper $ AttachedToEnemy tzuSanNiang
      t <- getTime
      if
        | t <= 11 -> pure ()
        | t <= 18 -> placeTokens attrs theShadeReaper #charge 3
        | t <= 26 -> placeTokens attrs theShadeReaper #charge 6
        | otherwise -> do
            placeTokens attrs theShadeReaper #charge 9
            placeDoomOnAgenda 1
      pure s
    ResolveChaosToken _ Cultist _iid -> do
      when (isHardExpert attrs) do
        theShadeReaper <- selectJust $ scarletKeyIs Keys.theShadeReaper
        placeTokens Cultist theShadeReaper #charge 1
      pure s
    ResolveChaosToken _ Tablet iid -> do
      whenM (matches iid (InvestigatorAt $ LocationWithEnemy $ EnemyWithTrait Geist)) do
        drawAnotherChaosToken iid
      pure s
    ResolveChaosToken drawnToken ElderThing iid -> do
      chooseOneM iid do
        when (isEasyStandard attrs) $ labeled' "elderThing.easyStandard" do
          chaosTokenEffect ElderThing drawnToken $ ChaosTokenFaceModifier [Zero]
        when (isHardExpert attrs) $ labeled' "elderThing.hardExpert" do
          chaosTokenEffect ElderThing drawnToken $ ChaosTokenFaceModifier [MinusThree]
        unscoped skip_
      pure s
    FailedSkillTest _iid _ _ (ChaosTokenTarget token) _ n -> do
      case token.face of
        Cultist | isEasyStandard attrs && n >= 2 -> do
          theShadeReaper <- selectJust $ scarletKeyIs Keys.theShadeReaper
          placeTokens Cultist theShadeReaper #charge 1
        _ -> pure ()
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          record YouHaventSeenTheLastOfTzuSanNiang
          setBearer Keys.theShadeReaper $ keyWithEnemy Enemies.tzuSanNiangAWhisperInYourEar
          resolutionWithXp "noResolution" $ allGainXp' attrs
        Resolution 1 -> do
          record TzuSanNiangIsUnderYourSway
          resolutionWithXp "resolution1" $ allGainXpWithBonus' attrs $ toBonus "bonus" 1
          eachInvestigator (`sufferPhysicalTrauma` 1)
          chooseBearer Keys.theShadeReaper
        Resolution 2 -> do
          record YouHaventSeenTheLastOfTzuSanNiang
          resolutionWithXp "resolution2" $ allGainXp' attrs
          chooseBearer Keys.theShadeReaper
        Resolution 3 -> do
          record TzuSanNiangHasYouUnderHerSway
          setBearer Keys.theShadeReaper $ keyWithEnemy Enemies.tzuSanNiangAWhisperInYourEar
          resolutionWithXp "resolution3" $ allGainXp' attrs
        _ -> error "Unknown resolution for Shades of Suffering"
      markTime 1
      endOfScenario
      pure s
    _ -> ShadesOfSuffering <$> liftRunMessage msg attrs
