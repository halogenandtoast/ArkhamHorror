module Arkham.Scenario.Scenarios.TheWitchingHour (TheWitchingHour (..), theWitchingHour) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Sequence (ActStep (..), actStep)
import Arkham.Act.Types (Field (ActSequence))
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheCircleUndone.ChaosBag
import Arkham.Campaigns.TheCircleUndone.Memento
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Helpers.Xp
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.TheWitchingHour.Helpers
import Arkham.Scenarios.TheWitchingHour.Story
import Data.Map.Monoidal qualified as MonoidalMap
import Data.Map.Strict qualified as Map

newtype TheWitchingHour = TheWitchingHour ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theWitchingHour :: Difficulty -> TheWitchingHour
theWitchingHour difficulty =
  scenario
    TheWitchingHour
    "05050"
    "The Witching Hour"
    difficulty
    [ ".      .     woods1        .      . "
    , ".      .     .             .      . "
    , "woods2 .     witchesCircle .      woods3"
    , ".      .     .             .      ."
    , ".      wood4 .             woods5 ."
    ] -- lost and separated, do we label 4 zones, or do a different placement

instance HasChaosTokenValue TheWitchingHour where
  getChaosTokenValue iid chaosTokenFace (TheWitchingHour attrs) = case chaosTokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 1 2
    Tablet -> pure $ toChaosTokenValue attrs Skull 1 2
    ElderThing -> pure $ toChaosTokenValue attrs Skull 3 4
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage TheWitchingHour where
  runMessage msg s@(TheWitchingHour attrs) = runQueueT $ scenarioI18n $ case msg of
    StandaloneSetup -> do
      setChaosTokens $ chaosBagContents attrs.difficulty
      pure s
    PreScenarioSetup -> do
      story intro1

      lead <- getLead
      chooseOneM lead do
        labeled "“What can I do to avoid this fate?”" $ doStep 2 PreScenarioSetup
        labeled "“This is bullshit.”" $ doStep 3 PreScenarioSetup
      pure s
    DoStep 2 PreScenarioSetup -> do
      story intro2
      record YouHaveAcceptedYourFate
      addChaosToken Tablet
      addChaosToken Tablet
      -- collection is infinite so we only care if the lead already has either card in their deck
      lead <- getLead
      addCards <-
        fieldMap
          InvestigatorDeck
          (not . any ((`elem` [Assets.theTowerXVI, Assets.aceOfRods1]) . toCardDef))
          lead
      when addCards do
        addCampaignCardToDeck lead Assets.theTowerXVI
        addCampaignCardToDeck lead Assets.aceOfRods1
      pure s
    DoStep 3 PreScenarioSetup -> do
      story intro3
      record YouHaveRejectedYourFate
      addChaosToken ElderThing
      addChaosToken ElderThing
      pure s
    DoStep 4 PreScenarioSetup -> do
      story intro4
      pure s
    Setup -> runScenarioSetup TheWitchingHour attrs do
      -- The Devourer Below is only locations
      gather Set.TheWitchingHour
      gather Set.AnettesCoven
      gather Set.CityOfSins
      gather Set.Witchcraft
      gather Set.AncientEvils
      gather Set.StrikingFear

      gatherAndSetAside Set.AgentsOfShubNiggurath
      gatherAndSetAside Set.AgentsOfAzathoth

      witchHauntedWoods <-
        sampleN 5
          $ Locations.witchHauntedWoodsAbandonedMine
          :| [ Locations.witchHauntedWoodsCairnStones
             , Locations.witchHauntedWoodsTheLonelyTree
             , Locations.witchHauntedWoodsChildsTreeHouse
             , Locations.witchHauntedWoodsTaintedWell
             , Locations.witchHauntedWoodsHermitsHouse
             , Locations.witchHauntedWoodsOvergrownBarn
             ]

      setAside
        [ Enemies.anetteMason
        , Locations.arkhamWoodsUnhallowedGround
        , Locations.arkhamWoodsTwistingPaths
        , Locations.arkhamWoodsOldHouse
        , Locations.arkhamWoodsCliffside
        , Locations.arkhamWoodsTangledThicket
        , Locations.arkhamWoodsQuietGlade
        ]

      iids <- getInvestigators
      let
        woodsWithInvestigators = zip (cycleN 5 iids) witchHauntedWoods
        locationMap =
          foldMap
            (\(investigator, location) -> MonoidalMap.singleton investigator (location :| []))
            woodsWithInvestigators
      startingLocations <-
        Map.fromList
          <$> traverse
            (\(k, v) -> (k,) <$> sample v)
            (MonoidalMap.toList locationMap)

      for_ woodsWithInvestigators $ \(investigator, location) -> do
        lid <- place location
        push $ PutLocationInFrontOf investigator lid
        runMaybeT do
          startingLocation <- hoistMaybe $ lookup investigator startingLocations
          guard $ location == startingLocation
          lift $ moveTo_ attrs investigator lid

      setAgendaDeck [Agendas.temperanceXIV, Agendas.theNightHowls]
      setActDeck
        [Acts.lostInTheWoods, Acts.witchHauntings, Acts.pathsIntoTwilight, Acts.aCircleUnbroken]
    ScenarioResolution resolution -> do
      step <- actStep <$> selectJustField ActSequence AnyAct
      case resolution of
        NoResolution -> push $ if step == ActStep 4 then R4 else R3
        Resolution 1 -> do
          story resolution1
          record TheWitches'SpellWasBroken
          recordSetInsert MementosDiscovered [MesmerizingFlute, RitualComponents]
          allGainXpWithBonus attrs $ toBonus "bonus" 1
          endOfScenario
        Resolution 2 -> do
          story resolution2
          record TheWitches'SpellWasBroken
          recordSetInsert MementosDiscovered [MesmerizingFlute, ScrapOfTornShadow]
          allGainXpWithBonus attrs $ toBonus "bonus" 1
          endOfScenario
        Resolution 3 -> do
          story resolution3
          record TheWitches'SpellWasCast
          if step == ActStep 3
            then do
              recordSetInsert MementosDiscovered [MesmerizingFlute]
              allGainXpWithBonus attrs $ toBonus "bonus" 1
            else allGainXp attrs
          endOfScenario
        Resolution 4 -> do
          story resolution4
          record TheWitches'SpellWasCast
          recordSetInsert MementosDiscovered [MesmerizingFlute]
          allGainXpWithBonus attrs $ toBonus "bonus" 1
          endOfScenario
        _ -> error "invalid resolution"
      pure s
    _ -> TheWitchingHour <$> liftRunMessage msg attrs
