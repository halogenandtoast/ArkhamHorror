module Arkham.Scenario.Scenarios.InTheClutchesOfChaos (InTheClutchesOfChaos (..), inTheClutchesOfChaos) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.CampaignLogKey
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Log
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Location.BreachStatus
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.InTheClutchesOfChaos.Helpers
import Arkham.Scenarios.InTheClutchesOfChaos.Story
import Arkham.Trait qualified as Trait
import Arkham.Treachery.Cards qualified as Treacheries

newtype InTheClutchesOfChaos = InTheClutchesOfChaos ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inTheClutchesOfChaos :: Difficulty -> InTheClutchesOfChaos
inTheClutchesOfChaos difficulty =
  scenario
    InTheClutchesOfChaos
    "05284"
    "In the Clutches of Chaos"
    difficulty
    [ ".            .            .      merchantDistrict merchantDistrict rivertown   rivertown  .          .                   ."
    , "hangmansHill hangmansHill uptown uptown           southside        southside   frenchHill frenchHill silverTwilightLodge silverTwilightLodge"
    , ".            .            .      .                southChurch      southChurch .          .          .                   ."
    ]

instance HasChaosTokenValue InTheClutchesOfChaos where
  getChaosTokenValue iid tokenFace (InTheClutchesOfChaos attrs) = case tokenFace of
    Skull -> do
      doom <- getSum <$> selectAgg Sum LocationDoom (locationWithInvestigator iid)
      breaches <-
        sum . map (maybe 0 countBreaches) <$> selectFields LocationBreaches (locationWithInvestigator iid)
      pure $ toChaosTokenValue attrs Skull (doom + breaches) (doom + breaches + 1)
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ toChaosTokenValue attrs Tablet 2 3
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 4
    otherFace -> getChaosTokenValue iid otherFace attrs

standaloneChaosTokens :: [ChaosTokenFace]
standaloneChaosTokens =
  [ PlusOne
  , Zero
  , Zero
  , MinusOne
  , MinusOne
  , MinusTwo
  , MinusTwo
  , MinusThree
  , MinusFour
  , Skull
  , Skull
  , Cultist
  , Tablet
  , ElderThing
  , AutoFail
  , ElderSign
  ]

instance RunMessage InTheClutchesOfChaos where
  runMessage msg s@(InTheClutchesOfChaos attrs) = runQueueT $ case msg of
    PreScenarioSetup -> do
      story intro1

      neverSeenOrHeardFromAgain <- getHasRecord TheInvestigatorsAreNeverSeenOrHeardFromAgain
      doStep (if neverSeenOrHeardFromAgain then 2 else 3) PreScenarioSetup
      pure s
    DoStep 2 PreScenarioSetup -> do
      story intro2
      doStep 4 PreScenarioSetup
      pure s
    DoStep 3 PreScenarioSetup -> do
      story intro3
      doStep 4 PreScenarioSetup
      pure s
    DoStep 4 PreScenarioSetup -> do
      story intro4
      pure s
    StandaloneSetup -> do
      setChaosTokens standaloneChaosTokens
      lead <- getLead
      chooseOneM lead do
        labeled "Anette Mason is possessed by evil." $ record AnetteMasonIsPossessedByEvil
        labeled "Carl Sanford possesses the secrets of the universe."
          $ record CarlSanfordPossessesTheSecretsOfTheUniverse
      pure s
    Setup -> runScenarioSetup InTheClutchesOfChaos attrs do
      gather Set.InTheClutchesOfChaos
      gather Set.AgentsOfAzathoth
      gather Set.Nightgaunts

      anetteMasonIsPossessedByEvil <- getHasRecord AnetteMasonIsPossessedByEvil
      when anetteMasonIsPossessedByEvil do
        gather Set.MusicOfTheDamned
        gather Set.AnettesCoven
        gather Set.CityOfSins
        gather Set.Witchcraft

      carlSanfordPossessesTheSecretsOfTheUniverse <-
        getHasRecord CarlSanfordPossessesTheSecretsOfTheUniverse
      when carlSanfordPossessesTheSecretsOfTheUniverse do
        gather Set.SecretsOfTheUniverse
        gather Set.SilverTwilightLodge
        gather Set.StrikingFear
        gatherJust Set.TheMidnightMasks [Treacheries.huntingShadow, Treacheries.falseLead]

      startAt =<< placeOneOf (Locations.southside_294 :| [Locations.southside_295])
      placeOneOf_ $ Locations.frenchHill_290 :| [Locations.frenchHill_291]
      placeOneOf_ $ Locations.rivertown_292 :| [Locations.rivertown_293]
      placeOneOf_ $ Locations.uptown_296 :| [Locations.uptown_297]
      placeOneOf_ $ Locations.southChurch_298 :| [Locations.southChurch_299]
      placeOneOf_ $ Locations.merchantDistrict_300 :| [Locations.merchantDistrict_301]

      if anetteMasonIsPossessedByEvil
        then do
          placeAll [Locations.hangmansHillWhereItAllEnds, Locations.silverTwilightLodgeShroudedInMystery]
          removeOneOfEach
            [Locations.hangmansHillShroudedInMystery, Locations.silverTwilightLodgeWhereItAllEnds]
        else do
          placeAll [Locations.hangmansHillShroudedInMystery, Locations.silverTwilightLodgeWhereItAllEnds]
          removeOneOfEach
            [Locations.hangmansHillWhereItAllEnds, Locations.silverTwilightLodgeShroudedInMystery]

      setAgendaDeck [Agendas.theChariotVII]
      setActDeck
        $ if anetteMasonIsPossessedByEvil
          then [Acts.darkKnowledgeV1, Acts.beyondTheGrave]
          else [Acts.darkKnowledgeV2, Acts.newWorldOrder]

      setAside [Enemies.piperOfAzathoth]
      push $ SetupStep (toTarget attrs) 1
    SetupStep (isTarget attrs -> True) 1 -> do
      playerCount <- getPlayerCount
      if playerCount == 4
        then replicateM_ 3 $ do
          lids <- sampleLocations 3
          pushAll [PlaceBreaches (toTarget lid) 1 | lid <- lids]
        else replicateM_ playerCount $ do
          lids <- sampleLocations 2
          pushAll [PlaceBreaches (toTarget lid) 1 | lid <- lids]
      pure s
    ResolveChaosToken _ Cultist iid -> do
      push $ DrawAnotherChaosToken iid
      mLocation <- selectOne $ locationWithInvestigator iid
      for_ mLocation $ \location -> do
        n <- getBreaches location
        pushWhen (n < 3) $ PlaceBreaches (toTarget location) 1
      pure s
    FailedSkillTest _iid _ _ (ChaosTokenTarget token) _ n -> do
      act <- selectJust AnyAct
      case token.face of
        Tablet -> push $ RemoveBreaches (toTarget act) n
        ElderThing -> do
          lid <- sampleLocation
          push $ PlaceBreaches (toTarget lid) 1
        _ -> pure ()
      pure s
    ScenarioResolution n -> do
      lead <- getLead
      anyDetectivePoliceOrAgency <-
        selectAny $ mapOneOf InvestigatorWithTrait [Trait.Detective, Trait.Police, Trait.Agency]
      case n of
        NoResolution -> do
          anetteMasonIsPossessedByEvil <- getHasRecord AnetteMasonIsPossessedByEvil
          push $ if anetteMasonIsPossessedByEvil then R3 else R4
        Resolution 1 -> do
          anySorcererMiskatonicOrScholar <-
            selectAny
              $ AnyInvestigator
              $ map InvestigatorWithTrait [Trait.Sorcerer, Trait.Miskatonic, Trait.Scholar]
          story resolution1
          chooseOneM lead do
            labeled "“You’ve done enough harm. We’ll handle this from here.”"
              $ record TheInvestigatorsContinuedAlone
            labeled "“We will need your help to fix this.” " $ record TheInvestigatorsAskedAnetteForAssistance

            when anyDetectivePoliceOrAgency
              $ labeled "“You are under arrest.”"
              $ record TheInvestigatorsArrestedAnette

            when anySorcererMiskatonicOrScholar
              $ labeled "“Then teach me how to be stronger.”"
              $ record AnetteTaughtYouTheSpellsOfOld

          allGainXp attrs
          endOfScenario
        Resolution 2 -> do
          anySorcererSilverTwilightOrCultist <-
            selectAny $ mapOneOf InvestigatorWithTrait [Trait.Sorcerer, Trait.SilverTwilight, Trait.Cultist]
          story resolution2
          chooseOneM lead do
            labeled "“You’ve done enough harm. We’ll handle this from here.”"
              $ record TheInvestigatorsContinuedAlone
            labeled "“We will need your help to fix this.” " $ record TheInvestigatorsAskedSanfordForAssistance
            when anyDetectivePoliceOrAgency
              $ labeled "“You are under arrest.”"
              $ record TheInvestigatorsArrestedSanford
            when anySorcererSilverTwilightOrCultist
              $ labeled "“Then teach me how to be stronger.”"
              $ record TheInvestigatorsAssumedControlOfTheSilverTwilightLodge
          allGainXp attrs
          endOfScenario
        Resolution 3 -> do
          story resolution3
          record DoomDrawsEverCloser
          allGainXp attrs
          endOfScenario
        Resolution 4 -> do
          story resolution4
          record DoomDrawsEverCloser
          allGainXp attrs
          endOfScenario
        _ -> error "no such resolution"
      pure s
    _ -> InTheClutchesOfChaos <$> liftRunMessage msg attrs
