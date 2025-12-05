module Arkham.Scenario.Scenarios.OnThinIce (onThinIce) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Campaigns.TheScarletKeys.CampaignSteps qualified as CS
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Campaigns.TheScarletKeys.Key
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Keys
import Arkham.Campaigns.TheScarletKeys.Key.Matcher
import Arkham.Campaigns.TheScarletKeys.Meta
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Query (allInvestigators)
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.OnThinIce.Helpers
import Arkham.Strategy
import Arkham.Trait (Trait (Hazard, Wayfarer, Wilderness))
import Data.Map.Strict qualified as Map

data InvestigatorData = InvestigatorData
  { physicalTrauma :: Int
  , mentalTrauma :: Int
  , xp :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype OnThinIceMeta = OnThinIceMeta (Map InvestigatorId InvestigatorData)
  deriving newtype (FromJSON, ToJSON)

newtype OnThinIce = OnThinIce ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onThinIce :: Difficulty -> OnThinIce
onThinIce difficulty =
  scenario
    OnThinIce
    "09609"
    "On Thin Ice"
    difficulty
    [ ".         .         alaskanWilderness1 .             outerWilderness1"
    , "anchorage fairbanks alaskanWilderness2 .             outerWilderness2"
    , ".         .         alaskanWilderness3 .             outerWilderness3"
    , ".         .         .                  outsidersLair ."
    ]

instance HasChaosTokenValue OnThinIce where
  getChaosTokenValue iid tokenFace (OnThinIce attrs) = case tokenFace of
    Skull -> do
      n <- selectCount $ TreacheryWithTrait Hazard
      pure $ toChaosTokenValue attrs Skull n (n + 1)
    Cultist -> pure $ toChaosTokenValue attrs Cultist 4 5
    Tablet -> do
      chimera <- selectAny $ EnemyWithTitle "Void Chimera" <> enemyAtLocationWith iid
      pure $ toChaosTokenValue attrs Tablet (if chimera then 5 else 2) 3
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 4
    otherFace -> getChaosTokenValue iid otherFace attrs

instance HasModifiersFor OnThinIce where
  getModifiersFor (OnThinIce attrs) = do
    modifySelect
      attrs
      (not_ $ LocationWithTrait Wilderness)
      [CampaignModifier "noConcealed[VoidChimeraTrueForm]"]

instance RunMessage OnThinIce where
  runMessage msg s@(OnThinIce attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      wayfarer <- selectAny $ InvestigatorWithTrait Wayfarer
      flavor do
        setTitle "title"
        p "introPart1"
        p.validate wayfarer "wayfarer"
        p "introPart2"
      setupKeys
      investigators <- allInvestigators
      investigatorData <- for investigators \iid -> do
        physicalTrauma <- field InvestigatorPhysicalTrauma iid
        mentalTrauma <- field InvestigatorMentalTrauma iid
        xp <- field InvestigatorXp iid
        pure (iid, InvestigatorData {..})
      pure $ OnThinIce $ attrs & metaL .~ toJSON (OnThinIceMeta (Map.fromList investigatorData))
    Setup -> runScenarioSetup OnThinIce attrs do
      setup $ ul do
        li "gatherSets"
        li "placeLocations"
        li "wilderness"
        li "setOutOfPlay"
        li "miniCards"
        unscoped $ li "shuffleRemainder"
        li "time"
        unscoped $ li "readyToBegin"
      gather Set.OnThinIce
      gather Set.AgentsOfTheOutside
      gather Set.AgentsOfYuggoth
      gather Set.CrimsonConspiracy
      gather Set.DarkVeiling
      gather Set.Outsiders
      gather Set.SpatialAnomaly
      gather Set.ChillingCold
      handleRedCoterie

      setAgendaDeck [Agendas.lostAndForgotten, Agendas.eyesOfTheVoid, Agendas.annihilation]
      setActDeck [Acts.questForTheSableGlass, Acts.ifItBleeds, Acts.prowlingNightmare, Acts.lastStand]

      startAt =<< place Locations.anchorage
      place_ Locations.fairbanks

      placeGroup
        "alaskanWilderness"
        [Locations.mountainStream, Locations.frozenLake, Locations.isolatedRoad]

      setAside
        [ Keys.theSableGlass
        , Enemies.thorneTheOneWithTheRedCravat
        , Enemies.voidChimeraTrueForm
        , Enemies.voidChimeraFellbeak
        , Enemies.voidChimeraEarsplitter
        , Enemies.voidChimeraGorefeaster
        , Enemies.voidChimeraFellhound
        , Locations.forgottenOutpost
        , Locations.huntersLodge
        , Locations.condemnedGoldMine
        ]

      t <- getTime
      let n = t `div` 5
      placeDoomOnAgenda n
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        Cultist | isEasyStandard attrs -> do
          cards <- select $ basic NonWeakness <> inHandOf NotForPlay iid
          focusCards cards $ chooseTargetM iid cards $ hollow iid
        ElderThing | isEasyStandard attrs -> do
          lookAt iid ElderThing iid [fromTopOfDeck 1] #any (defer ElderThing IsNotDraw)
        _ -> pure ()
      pure s
    ResolveChaosToken _ Cultist iid -> do
      when (isHardExpert attrs) do
        cards <- select $ basic NonWeakness <> inHandOf NotForPlay iid
        focusCards cards $ chooseTargetM iid cards $ hollow iid
      pure s
    ResolveChaosToken _ Tablet iid -> do
      when (isHardExpert attrs) do
        chimera <- selectAny $ EnemyWithTitle "Void Chimera" <> enemyAtLocationWith iid
        when chimera failSkillTest
      pure s
    ResolveChaosToken _ ElderThing iid -> do
      when (isHardExpert attrs) do
        lookAt iid ElderThing iid [fromTopOfDeck 1] #any (defer ElderThing IsNotDraw)
      pure s
    SearchFound iid (ChaosTokenFaceTarget ElderThing) _ cards | notNull cards -> do
      focusCards cards do
        for_ cards \card -> do
          when (cardMatch card NonWeakness) $ hollow iid card
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          record TheVoidChimeraEscaped
          madeADeal <- getHasRecord TheCellMadeADealWithThorne
          mcontroller <- selectOne $ InvestigatorWithScarletKey $ scarletKeyIs Keys.theSableGlass
          case mcontroller of
            Just controller | not madeADeal -> do
              setBearer Keys.theSableGlass $ KeyWithInvestigator controller
              record YouHaventSeenTheLastOfThorne
            _ -> setBearer Keys.theSableGlass $ keyWithEnemy Enemies.thorneOpenToNegotiation
          resolutionWithXp "noResolution" $ allGainXp' attrs
          markTime 1
          endOfScenario
        Resolution 1 -> do
          setBearer Keys.theSableGlass $ keyWithEnemy Enemies.thorneOpenToNegotiation
          resolutionWithXp "resolution1" $ allGainXp' attrs
          markTime 3
          endOfScenario
        Resolution 2 -> do
          record YouHaventSeenTheLastOfThorne
          mcontroller <- selectOne $ InvestigatorWithScarletKey $ scarletKeyIs Keys.theSableGlass
          for_ mcontroller $ setBearer Keys.theSableGlass . KeyWithInvestigator
          resolutionWithXp "resolution2" $ allGainXp' attrs
          markTime 3
          endOfScenario
        Resolution 3 -> do
          record ThorneDisappeared
          mcontroller <- selectOne $ InvestigatorWithScarletKey $ scarletKeyIs Keys.theSableGlass
          setBearer Keys.theSableGlass $ case mcontroller of
            Just controller -> KeyWithInvestigator controller
            _ -> keyWithEnemy Enemies.thorneOpenToNegotiation
          resolutionWithXp "resolution3" $ allGainXp' attrs
          markTime 3
          endOfScenario
        Resolution 4 -> do
          push $ IgnoreGainXP CS.OnThinIce
          record ThereIsNothingOfNoteInAnchorage
          resolution "resolution4"
          let OnThinIceMeta metaMap = toResultDefault @OnThinIceMeta (OnThinIceMeta mempty) attrs.meta
          for_ (Map.assocs metaMap) \(iid, InvestigatorData {..}) -> do
            push $ SetTrauma iid physicalTrauma mentalTrauma
            push $ SetXP iid xp
          endOfScenario
        _ -> error "Unexpected resolution"
      pure s
    _ -> OnThinIce <$> liftRunMessage msg attrs
