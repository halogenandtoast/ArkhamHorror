module Arkham.Scenario.Scenarios.TheUntamedWilds (TheUntamedWilds (..), theUntamedWilds) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Sequence qualified as AS
import Arkham.Act.Types
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheForgottenAge.ChaosBag
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Meta
import Arkham.Classes
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Game.Helpers hiding (checkWhen)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Helpers hiding (addCampaignCardToDeckChoice, checkWhen)
import Arkham.Scenario.Import.Lifted
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TheUntamedWilds.Story
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Window qualified as Window

newtype TheUntamedWilds = TheUntamedWilds ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theUntamedWilds :: Difficulty -> TheUntamedWilds
theUntamedWilds difficulty =
  scenario
    TheUntamedWilds
    "04043"
    "The Untamed Wilds"
    difficulty
    [ ".               .             ruinsOfEztli   .               ."
    , ".               serpentsHaven ruinsOfEztli   circuitousTrail ."
    , "templeOfTheFang serpentsHaven riverCanyon    circuitousTrail overgrownRuins"
    , "templeOfTheFang pathOfThorns  riverCanyon    ropeBridge      overgrownRuins"
    , ".               pathOfThorns  expeditionCamp ropeBridge      ."
    , ".               .             expeditionCamp .               ."
    ]

instance HasChaosTokenValue TheUntamedWilds where
  getChaosTokenValue iid chaosTokenFace (TheUntamedWilds attrs) = case chaosTokenFace of
    Skull -> do
      vengeance <- getVengeanceInVictoryDisplay
      pure $ toChaosTokenValue attrs Skull vengeance (vengeance + 1)
    Cultist -> do
      locationCount <- selectCount Anywhere
      pure $ toChaosTokenValue attrs Cultist (min 5 locationCount) locationCount
    Tablet -> do
      explorationDeckCount <- length <$> getExplorationDeck
      pure $ toChaosTokenValue attrs Tablet (min 5 explorationDeckCount) (max 3 explorationDeckCount)
    ElderThing -> do
      isPoisoned <- getIsPoisoned iid
      if isPoisoned
        then pure $ ChaosTokenValue ElderThing AutoFailModifier
        else pure $ toChaosTokenValue attrs ElderThing 2 3
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage TheUntamedWilds where
  runMessage msg s@(TheUntamedWilds attrs) = runQueueT $ case msg of
    PreScenarioSetup -> do
      story intro
      pure s
    StandaloneSetup -> do
      setChaosTokens $ chaosBagContents attrs.difficulty
      pure s
    Setup -> runScenarioSetup TheUntamedWilds attrs do
      gather Set.TheUntamedWilds
      gather Set.Rainforest
      gather Set.Serpents
      gather Set.Expedition
      gather Set.GuardiansOfTime
      gather Set.Poison
      gather Set.AncientEvils
      startAt =<< place Locations.expeditionCamp

      gatherAndSetAside Set.AgentsOfYig
      setAside
        $ [ Locations.ruinsOfEztli
          , Locations.templeOfTheFang
          , Locations.overgrownRuins
          , Assets.alejandroVela
          , Enemies.ichtaca
          , Treacheries.poisoned
          , Treacheries.poisoned
          , Treacheries.poisoned
          , Treacheries.poisoned
          ]

      setAgendaDeck [Agendas.expeditionIntoTheWild, Agendas.intruders]
      setActDeck
        [ Acts.exploringTheRainforest
        , Acts.huntressOfTheEztli
        , Acts.searchForTheRuins
        , Acts.theGuardedRuins
        ]

      addExtraDeck ExplorationDeck
        =<< shuffle
          [ Locations.pathOfThorns
          , Locations.riverCanyon
          , Locations.ropeBridge
          , Locations.serpentsHaven
          , Locations.circuitousTrail
          , Treacheries.lostInTheWilds
          , Treacheries.overgrowth
          , Treacheries.snakeBite
          , Treacheries.lowOnSupplies
          , Treacheries.arrowsFromTheTrees
          ]
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        ElderThing | isHardExpert attrs -> do
          isPoisoned <- getIsPoisoned iid
          unless isPoisoned $ do
            poisoned <- getSetAsidePoisoned
            push $ CreateWeaknessInThreatArea poisoned iid
        _ -> pure ()
      pure s
    Explore iid _ _ -> do
      checkWhen $ Window.AttemptExplore iid
      push $ Do msg
      pure s
    Do (Explore iid source locationMatcher) -> do
      explore iid source locationMatcher PlaceExplored 1
      pure s
    ScenarioResolution res -> do
      investigators <- allInvestigators
      actStep <- fieldMap ActSequence (AS.unActStep . AS.actStep) =<< selectJust AnyAct
      vengeance <- getVengeanceInVictoryDisplay
      case res of
        NoResolution -> do
          story noResolution
          record TheInvestigatorsWereForcedToWaitForAdditionalSupplies
          recordWhen (actStep < 3) IchtacaObservedYourProgressWithKeenInterest
          foughtWithIchtaca <- remembered YouFoughtWithIchtaca
          recordWhen foughtWithIchtaca IchtacaIsWaryOfTheInvestigators
          recordWhen (actStep < 3 || foughtWithIchtaca) AlejandroFollowedTheInvestigatorsIntoTheRuins
          addCampaignCardToDeckChoice investigators Assets.alejandroVela
          whenM (remembered IchtachaIsLeadingTheWay) do
            record TheInvestigatorsHaveEarnedIchtacasTrust
            record AlejandroChoseToRemainAtCamp
          recordCount YigsFury vengeance
          allGainXp attrs
          endOfScenario
        Resolution 1 -> do
          story resolution1
          record TheInvestigatorsClearedAPathToTheEztliRuins
          record AlejandroChoseToRemainAtCamp
          record TheInvestigatorsHaveEarnedIchtacasTrust
          recordCount YigsFury vengeance
          allGainXp attrs
          endOfScenario
        Resolution 2 -> do
          story resolution2
          record TheInvestigatorsClearedAPathToTheEztliRuins
          record AlejandroFollowedTheInvestigatorsIntoTheRuins
          addCampaignCardToDeckChoice investigators Assets.alejandroVela
          record IchtacaIsWaryOfTheInvestigators
          recordCount YigsFury vengeance
          allGainXp attrs
          endOfScenario
        _ -> error "invalid resolution"
      pure s
    ChooseLeadInvestigator -> do
      standalone <- getIsStandalone
      leader <- if standalone then pure Nothing else expeditionLeader <$> getCampaignMeta
      case leader of
        Just iid -> do
          push $ ChoosePlayer iid SetLeadInvestigator
          pure s
        Nothing -> TheUntamedWilds <$> liftRunMessage msg attrs
    _ -> TheUntamedWilds <$> liftRunMessage msg attrs
