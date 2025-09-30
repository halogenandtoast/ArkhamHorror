module Arkham.Scenario.Scenarios.InTheClutchesOfChaos (setupInTheClutchesOfChaos, inTheClutchesOfChaos, InTheClutchesOfChaos (..)) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Campaigns.TheCircleUndone.Key
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Log
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Location.BreachStatus
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.InTheClutchesOfChaos.Helpers
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
      doom <- selectSum LocationDoom (locationWithInvestigator iid)
      breaches <-
        sum . map (maybe 0 countBreaches) <$> selectFields LocationBreaches (locationWithInvestigator iid)
      pure $ toChaosTokenValue attrs Skull (doom + breaches) (doom + breaches + 1)
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ toChaosTokenValue attrs Tablet 2 3
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 4
    otherFace -> getChaosTokenValue iid otherFace attrs

{- FOURMOLU_DISABLE -}
standaloneChaosTokens :: [ChaosTokenFace]
standaloneChaosTokens =
  [ PlusOne , Zero , Zero , MinusOne , MinusOne , MinusTwo , MinusTwo , MinusThree , MinusFour , Skull
  , Skull , Cultist , Tablet , ElderThing , AutoFail , ElderSign
  ]
{- FOURMOLU_ENABLE -}

setupInTheClutchesOfChaos :: (HasI18n, ReverseQueue m) => ScenarioAttrs -> ScenarioBuilderT m ()
setupInTheClutchesOfChaos attrs = do
  setup $ ul do
    li "gatherSets"
    li "placeLocations"
    scope "anetteMasonIsPossessedByEvil" do
      li.nested "checkCampaignLog" do
        li "gatherSets"
        li "actDeck"
        li "placeLocations"
    scope "carlSanfordPossessesTheSecretsOfTheUniverse" do
      li.nested "checkCampaignLog" do
        li "gatherSets"
        li "actDeck"
        li "placeLocations"
    li "setAside"
    scope "breaches" $ li.nested "instructions" do
      li "twoPlayers"
      li "threePlayers"
      li "fourPlayers"
    unscoped $ li "shuffleRemainder"
  gather Set.InTheClutchesOfChaos
  gather Set.AgentsOfAzathoth
  gather Set.Nightgaunts

  anetteMasonIsPossessedByEvil <- getHasRecord AnetteMasonIsPossessedByEvil
  when anetteMasonIsPossessedByEvil do
    gather Set.MusicOfTheDamned
    gather Set.AnettesCoven
    gather Set.CityOfSins
    gather Set.Witchcraft

  whenHasRecord CarlSanfordPossessesTheSecretsOfTheUniverse do
    gather Set.SecretsOfTheUniverse
    gather Set.SilverTwilightLodge
    gather Set.StrikingFear
    gatherJust Set.TheMidnightMasks [Treacheries.huntingShadow, Treacheries.falseLead]

  startAt =<< placeOneOf (Locations.southside_294, Locations.southside_295)
  placeOneOf_ (Locations.frenchHill_290, Locations.frenchHill_291)
  placeOneOf_ (Locations.rivertown_292, Locations.rivertown_293)
  placeOneOf_ (Locations.uptown_296, Locations.uptown_297)
  placeOneOf_ (Locations.southChurch_298, Locations.southChurch_299)
  placeOneOf_ (Locations.merchantDistrict_300, Locations.merchantDistrict_301)

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

instance RunMessage InTheClutchesOfChaos where
  runMessage msg s@(InTheClutchesOfChaos attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "intro1"
      neverSeenOrHeardFromAgain <- getHasRecord TheInvestigatorsAreNeverSeenOrHeardFromAgain
      doStep (if neverSeenOrHeardFromAgain then 2 else 3) PreScenarioSetup
      pure s
    DoStep 2 PreScenarioSetup -> scope "intro " do
      flavor $ setTitle "title" >> p "intro2"
      doStep 4 PreScenarioSetup
      pure s
    DoStep 3 PreScenarioSetup -> scope "intro " do
      flavor $ setTitle "title" >> p "intro3"
      doStep 4 PreScenarioSetup
      pure s
    DoStep 4 PreScenarioSetup -> scope "intro " do
      flavor $ setTitle "title" >> p "intro4"
      pure s
    StandaloneSetup -> scope "standalone" do
      setChaosTokens standaloneChaosTokens
      leadChooseOneM do
        labeled' "anetteMasonIsPossessedByEvil" $ record AnetteMasonIsPossessedByEvil
        labeled' "carlSanfordPossessesTheSecretsOfTheUniverse"
          $ record CarlSanfordPossessesTheSecretsOfTheUniverse
      pure s
    Setup -> runScenarioSetup InTheClutchesOfChaos attrs $ setupInTheClutchesOfChaos attrs
    SetupStep (isTarget attrs -> True) 1 -> do
      playerCount <- getPlayerCount
      if playerCount == 4
        then repeated 3 $ sampleLocations 3 >>= traverse_ (`placeBreaches` 1)
        else repeated playerCount $ sampleLocations 2 >>= traverse_ (`placeBreaches` 1)
      pure s
    ResolveChaosToken _ Cultist iid -> do
      drawAnotherChaosToken iid
      mLocation <- selectOne $ locationWithInvestigator iid
      for_ mLocation \location -> do
        n <- getBreaches location
        pushWhen (n < 3) $ PlaceBreaches (toTarget location) 1
      pure s
    FailedSkillTest _iid _ _ (ChaosTokenTarget token) _ n -> do
      case token.face of
        Tablet -> do
          act <- selectJust AnyAct
          push $ RemoveBreaches (toTarget act) n
        ElderThing -> do
          lid <- sampleLocation
          push $ PlaceBreaches (toTarget lid) 1
        _ -> pure ()
      pure s
    ScenarioResolution n -> scope "resolutions" do
      anyDetectivePoliceOrAgency <-
        selectAny $ mapOneOf InvestigatorWithTrait [Trait.Detective, Trait.Police, Trait.Agency]
      case n of
        NoResolution -> do
          resolution "noResolution"
          anetteMasonIsPossessedByEvil <- getHasRecord AnetteMasonIsPossessedByEvil
          push $ if anetteMasonIsPossessedByEvil then R3 else R4
        Resolution 1 -> do
          anySorcererMiskatonicOrScholar <-
            selectAny
              $ AnyInvestigator
              $ map InvestigatorWithTrait [Trait.Sorcerer, Trait.Miskatonic, Trait.Scholar]
          resolutionWithXp "resolution1" $ allGainXp' attrs
          leadChooseOneM do
            labeled' "continuedAlone" $ record TheInvestigatorsContinuedAlone
            labeled' "askedForAssistance" $ record TheInvestigatorsAskedAnetteForAssistance

            when anyDetectivePoliceOrAgency do
              labeled' "underArrest" $ record TheInvestigatorsArrestedAnette

            when anySorcererMiskatonicOrScholar do
              labeled' "spellsOfOld" $ record AnetteTaughtYouTheSpellsOfOld

          endOfScenario
        Resolution 2 -> do
          anySorcererSilverTwilightOrCultist <-
            selectAny $ mapOneOf InvestigatorWithTrait [Trait.Sorcerer, Trait.SilverTwilight, Trait.Cultist]
          resolutionWithXp "resolution2" $ allGainXp' attrs
          leadChooseOneM do
            labeled' "continuedAlone" $ record TheInvestigatorsContinuedAlone
            labeled' "askedForAssistance" $ record TheInvestigatorsAskedSanfordForAssistance
            when anyDetectivePoliceOrAgency do
              labeled' "underArrest" $ record TheInvestigatorsArrestedSanford
            when anySorcererSilverTwilightOrCultist do
              labeled' "assumedControl" $ record TheInvestigatorsAssumedControlOfTheSilverTwilightLodge
          endOfScenario
        Resolution 3 -> do
          resolutionWithXp "resolution3" $ allGainXp' attrs
          record DoomDrawsEverCloser
          endOfScenario
        Resolution 4 -> do
          resolutionWithXp "resolution4" $ allGainXp' attrs
          record DoomDrawsEverCloser
          endOfScenario
        _ -> error "no such resolution"
      pure s
    _ -> InTheClutchesOfChaos <$> liftRunMessage msg attrs
