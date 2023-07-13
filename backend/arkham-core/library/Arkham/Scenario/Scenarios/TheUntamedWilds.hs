module Arkham.Scenario.Scenarios.TheUntamedWilds (
  TheUntamedWilds (..),
  theUntamedWilds,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Sequence qualified as AS
import Arkham.Act.Types
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Game.Helpers
import Arkham.Helpers.Deck
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TheUntamedWilds.Story
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Window (Window (..))
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
      pure $
        toChaosTokenValue
          attrs
          Tablet
          (min 5 explorationDeckCount)
          (max 3 explorationDeckCount)
    ElderThing -> do
      isPoisoned <- getIsPoisoned iid
      if isPoisoned
        then pure $ ChaosTokenValue ElderThing AutoFailModifier
        else pure $ toChaosTokenValue attrs ElderThing 2 3
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage TheUntamedWilds where
  runMessage msg s@(TheUntamedWilds attrs) = case msg of
    Setup -> do
      investigatorIds <- allInvestigatorIds
      (expeditionCampId, placeExpeditionCamp) <-
        placeLocationCard
          Locations.expeditionCamp

      explorationDeck <-
        shuffleM
          =<< genCards
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
      agentsOfYig <-
        map EncounterCard
          <$> gatherEncounterSet EncounterSet.AgentsOfYig
      setAsideCards <-
        (agentsOfYig <>)
          <$> genCards
            [ Locations.ruinsOfEztli
            , Locations.templeOfTheFang
            , Locations.overgrownRuins
            , Assets.alejandroVela
            , Enemies.ichtaca
            , Treacheries.poisoned
            , Treacheries.poisoned
            , Treacheries.poisoned
            , Treacheries.poisoned
            ]
      encounterDeck <-
        buildEncounterDeckExcluding
          [ Enemies.ichtaca
          , Locations.pathOfThorns
          , Locations.riverCanyon
          , Locations.ropeBridge
          , Locations.serpentsHaven
          , Locations.circuitousTrail
          , Locations.ruinsOfEztli
          , Locations.templeOfTheFang
          , Locations.overgrownRuins
          ]
          [ EncounterSet.TheUntamedWilds
          , EncounterSet.Rainforest
          , EncounterSet.Serpents
          , EncounterSet.Expedition
          , EncounterSet.GuardiansOfTime
          , EncounterSet.Poison
          , EncounterSet.AncientEvils
          ]
      let
        encounterDeck' =
          removeEachFromDeck
            encounterDeck
            [ Treacheries.lostInTheWilds
            , Treacheries.overgrowth
            , Treacheries.snakeBite
            , Treacheries.lowOnSupplies
            , Treacheries.arrowsFromTheTrees
            ]
      pushAll $
        [ story investigatorIds intro
        , SetEncounterDeck encounterDeck'
        , SetAgendaDeck
        , SetActDeck
        , placeExpeditionCamp
        , MoveAllTo (toSource attrs) expeditionCampId
        ]

      agendas <- genCards [Agendas.expeditionIntoTheWild, Agendas.intruders]
      acts <-
        genCards
          [ Acts.exploringTheRainforest
          , Acts.huntressOfTheEztli
          , Acts.searchForTheRuins
          , Acts.theGuardedRuins
          ]
      TheUntamedWilds
        <$> runMessage
          msg
          ( attrs
              & (decksL . at ExplorationDeck ?~ explorationDeck)
              & (setAsideCardsL .~ setAsideCards)
              & (actStackL . at 1 ?~ acts)
              & (agendaStackL . at 1 ?~ agendas)
          )
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> case chaosTokenFace token of
      ElderThing | isHardExpert attrs -> do
        isPoisoned <- getIsPoisoned iid
        unless isPoisoned $ do
          poisoned <- getSetAsidePoisoned
          push $ CreateWeaknessInThreatArea poisoned iid
        pure s
      _ -> pure s
    Explore iid _ _ -> do
      windowMsg <- checkWindows [Window Timing.When $ Window.AttemptExplore iid]
      pushAll [windowMsg, Do msg]
      pure s
    Do (Explore iid source locationMatcher) -> do
      explore iid source locationMatcher PlaceExplored 1
      pure s
    ScenarioResolution res -> do
      investigatorIds <- allInvestigatorIds
      actStep <-
        fieldMap ActSequence (AS.unActStep . AS.actStep)
          =<< selectJust AnyAct
      xp <- getXp
      vengeance <- getVengeanceInVictoryDisplay
      leadInvestigatorId <- getLeadInvestigatorId
      case res of
        NoResolution -> do
          foughtWithIchtaca <- remembered YouFoughtWithIchtaca
          leadingTheWay <- remembered IchtachaIsLeadingTheWay
          pushAll $
            [ story investigatorIds noResolution
            , Record TheInvestigatorsWereForcedToWaitForAdditionalSupplies
            ]
              <> [ Record IchtacaObservedYourProgressWithKeenInterest
                 | actStep < 3
                 ]
              <> [Record IchtacaIsWaryOfTheInvestigators | foughtWithIchtaca]
              <> [ Record AlejandroFollowedTheInvestigatorsIntoTheRuins
                 | actStep < 3 || foughtWithIchtaca
                 ]
              <> [ addCampaignCardToDeckChoice
                    leadInvestigatorId
                    investigatorIds
                    Assets.alejandroVela
                 ]
              <> [ Record TheInvestigatorsHaveEarnedIchtacasTrust
                 | leadingTheWay
                 ]
              <> [Record AlejandroChoseToRemainAtCamp | leadingTheWay]
              <> [RecordCount YigsFury vengeance]
              <> [GainXP iid (toSource attrs) n | (iid, n) <- xp]
              <> [EndOfGame Nothing]
        Resolution 1 -> do
          pushAll $
            [ story investigatorIds resolution1
            , Record TheInvestigatorsClearedAPathToTheEztliRuins
            , Record AlejandroChoseToRemainAtCamp
            , Record TheInvestigatorsHaveEarnedIchtacasTrust
            , RecordCount YigsFury vengeance
            ]
              <> [GainXP iid (toSource attrs) n | (iid, n) <- xp]
              <> [EndOfGame Nothing]
        Resolution 2 -> do
          pushAll $
            [ story investigatorIds resolution2
            , Record TheInvestigatorsClearedAPathToTheEztliRuins
            , Record AlejandroFollowedTheInvestigatorsIntoTheRuins
            ]
              <> [ addCampaignCardToDeckChoice
                    leadInvestigatorId
                    investigatorIds
                    Assets.alejandroVela
                 ]
              <> [ Record IchtacaIsWaryOfTheInvestigators
                 , RecordCount YigsFury vengeance
                 ]
              <> [GainXP iid (toSource attrs) n | (iid, n) <- xp]
              <> [EndOfGame Nothing]
        _ -> error "invalid resolution"
      pure s
    _ -> TheUntamedWilds <$> runMessage msg attrs
