module Arkham.Scenario.Scenarios.TheUntamedWilds (setupTheUntamedWilds, theUntamedWilds, TheUntamedWilds (..)) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheForgottenAge.Import
import Arkham.Card.CardCode
import Arkham.Effect.Window
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers (Deck (..))
import Arkham.Helpers.Act
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query
import Arkham.I18n
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Helpers hiding (addCampaignCardToDeckChoice)
import Arkham.Scenario.Import.Lifted
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TheUntamedWilds.Helpers
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
    [ ".        .        hourglass .     ."
    , ".        triangle hourglass heart ."
    , "squiggle triangle diamond   heart t"
    , "squiggle square   diamond   moon  t"
    , ".        square   circle    moon  ."
    , ".        .        circle    .     ."
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

setupTheUntamedWilds :: (HasI18n, ReverseQueue m) => ScenarioAttrs -> ScenarioBuilderT m ()
setupTheUntamedWilds _attrs = do
  setup do
    ul do
      li "gatherSets"
      li "placeLocations"
      li "explorationDeck"
      li "setAside"
      unscoped $ li "shuffleRemainder"

  -- no return to set
  gather Set.TheUntamedWilds
  gather Set.Rainforest
  whenReturnTo $ gather Set.ReturnToRainforest

  gather Set.Serpents
  gather Set.Expedition `orWhenReturnTo` gather Set.DoomedExpedition
  gather Set.GuardiansOfTime
  gather Set.Poison
  gather Set.AncientEvils
  startAt =<< place Locations.expeditionCamp

  gatherAndSetAside Set.AgentsOfYig
  setAside
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

  setAgendaDeck [Agendas.expeditionIntoTheWild, Agendas.intruders]
  setActDeck
    [ Acts.exploringTheRainforest
    , Acts.huntressOfTheEztli
    , Acts.searchForTheRuins
    , Acts.theGuardedRuins
    ]

  square <- Locations.pathOfThorns `orSampleIfReturnTo` [Locations.riversideTemple]
  moon <- Locations.ropeBridge `orSampleIfReturnTo` [Locations.waterfall]
  triangle <- Locations.serpentsHaven `orSampleIfReturnTo` [Locations.trailOfTheDead]
  heart <- Locations.circuitousTrail `orSampleIfReturnTo` [Locations.cloudForest]

  isReturnTo <- getIsReturnTo

  let
    treacheries =
      guard (not isReturnTo)
        *> [ Treacheries.lostInTheWilds
           , Treacheries.overgrowth
           , Treacheries.snakeBite
           , Treacheries.lowOnSupplies
           , Treacheries.arrowsFromTheTrees
           ]

  addExtraDeck ExplorationDeck
    =<< shuffle ([square, Locations.riverCanyon, moon, triangle, heart] <> treacheries)

  whenReturnTo do
    addAdditionalReferences ["53016b"]
    createAbilityEffect EffectGameWindow
      $ mkAbility (SourceableWithCardCode (CardCode "53016b") ScenarioSource) 1
      $ forced
      $ Explored #after Anyone Anywhere (SuccessfulExplore Anywhere)

instance RunMessage TheUntamedWilds where
  runMessage msg s@(TheUntamedWilds attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      flavor $ scope "intro" $ h "title" >> p "body"
      pure s
    StandaloneSetup -> do
      setChaosTokens $ chaosBagContents attrs.difficulty
      pure s
    Setup -> runScenarioSetup TheUntamedWilds attrs $ setupTheUntamedWilds attrs
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        ElderThing | isHardExpert attrs -> do
          unlessM (getIsPoisoned iid) do
            poisoned <- getSetAsidePoisoned
            createWeaknessInThreatArea poisoned iid
        _ -> pure ()
      pure s
    Explore iid _ _ -> do
      checkWhen $ Window.AttemptExplore iid
      do_ msg
      pure s
    Do (Explore iid source locationMatcher) -> do
      explore iid source locationMatcher PlaceExplored 1
      pure s
    ScenarioResolution res -> scope "resolutions" do
      investigators <- allInvestigators
      case res of
        NoResolution -> do
          record TheInvestigatorsWereForcedToWaitForAdditionalSupplies
          actStep <- getCurrentActStep
          when (actStep < 3) do
            record IchtacaObservedYourProgressWithKeenInterest
            record AlejandroFollowedTheInvestigatorsIntoTheRuins
            addCampaignCardToDeckChoice investigators DoNotShuffleIn Assets.alejandroVela
          whenRemembered YouFoughtWithIchtaca do
            record IchtacaIsWaryOfTheInvestigators
            record AlejandroFollowedTheInvestigatorsIntoTheRuins
            addCampaignCardToDeckChoice investigators DoNotShuffleIn Assets.alejandroVela
          whenRemembered IchtachaIsLeadingTheWay do
            record TheInvestigatorsHaveEarnedIchtacasTrust
            record AlejandroChoseToRemainAtCamp
          resolutionWithXp "noResolution" $ allGainXp' attrs
        Resolution 1 -> do
          record TheInvestigatorsClearedAPathToTheEztliRuins
          record AlejandroChoseToRemainAtCamp
          record TheInvestigatorsHaveEarnedIchtacasTrust
          resolutionWithXp "resolution1" $ allGainXp' attrs
        Resolution 2 -> do
          record TheInvestigatorsClearedAPathToTheEztliRuins
          record AlejandroFollowedTheInvestigatorsIntoTheRuins
          addCampaignCardToDeckChoice investigators DoNotShuffleIn Assets.alejandroVela
          record IchtacaIsWaryOfTheInvestigators
          resolutionWithXp "resolution2" $ allGainXp' attrs
        _ -> error "invalid resolution"
      recordCountM YigsFury getTotalVengeanceInVictoryDisplay
      endOfScenario
      pure s
    ChooseLeadInvestigator -> do
      standalone <- getIsStandalone
      leader <- if standalone then pure Nothing else expeditionLeader <$> getCampaignMeta
      case leader of
        Just iid -> do
          push $ ChoosePlayer iid SetLeadInvestigator
          pure s
        Nothing -> TheUntamedWilds <$> liftRunMessage msg attrs
    UseCardAbility _ ScenarioSource 1 _ _ -> do
      getEncounterDeck >>= \case
        Deck [] -> pure ()
        Deck (x : _) -> shuffleCardsIntoDeck ExplorationDeck [x]
      pure s
    _ -> TheUntamedWilds <$> liftRunMessage msg attrs
