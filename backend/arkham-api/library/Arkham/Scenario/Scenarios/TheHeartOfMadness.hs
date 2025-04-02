module Arkham.Scenario.Scenarios.TheHeartOfMadness (theHeartOfMadness) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Campaigns.EdgeOfTheEarth.Key
import Arkham.Campaigns.EdgeOfTheEarth.Supplies
import Arkham.EncounterSet qualified as Set
import Arkham.FlavorText
import Arkham.Helpers.ChaosBag
import Arkham.Helpers.Modifiers (modifySelect, ModifierType(..))
import Arkham.Helpers.SkillTest
import Arkham.Helpers.Text
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move (moveAllTo)
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.TheHeartOfMadness.Helpers
import Arkham.Trait (Trait (Ancient))

newtype TheHeartOfMadness = TheHeartOfMadness ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHeartOfMadness :: Difficulty -> TheHeartOfMadness
theHeartOfMadness difficulty =
  scenario
    TheHeartOfMadness
    "08648"
    "The Heart of Madness"
    difficulty
    [ ".          .          .          facility1      .          .          ."
    , "facility2  .          .          facility1      .          .          facility3"
    , "facility2  .          .          facility4      .          .          facility3"
    , ".          facility5  .          facility4      .          facility6  ."
    , ".          facility5  .          facility7      .          facility6  ."
    , ".          .          facility8  facility7      facility9  .          ."
    , ".          .          facility8  theGateOfYquaa facility9  .          ."
    , ".          .          .          theGateOfYquaa .          .          ."
    , ".          .          facility10 .              facility11 .          ."
    , ".          .          facility10 .              facility11 .          ."
    , ".          facility12 .          .              .          facility13 ."
    , ".          facility12 .          .              .          facility13 ."
    , "facility14 .          .          .              .          .          facility15"
    , "facility14 .          .          .              .          .          facility15"
    ]

instance HasModifiersFor TheHeartOfMadness where
  getModifiersFor (TheHeartOfMadness _a) = do
    getSkillTestInvestigator >>= traverse_ \iid -> do
      whenM (sealAtLocationOf iid) do
        modifySelect Cultist (ChaosTokenFaceIs #cultist) [ChaosTokenFaceModifier [#frost]]

instance HasChaosTokenValue TheHeartOfMadness where
  getChaosTokenValue iid tokenFace (TheHeartOfMadness attrs) = case tokenFace of
    Skull -> do
      ancient <- selectAny $ withTrait Ancient <> EnemyAt (locationWithInvestigator iid)
      pure
        $ if ancient
          then toChaosTokenValue attrs Skull 1 3
          else toChaosTokenValue attrs Skull 2 4
    Cultist -> pure $ ChaosTokenValue Cultist (NegativeModifier 1)
    Tablet -> pure $ ChaosTokenValue Tablet (NegativeModifier 3)
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 4 5
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage TheHeartOfMadness where
  runMessage msg s@(TheHeartOfMadness attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      story $ i18nWithTitle "intro"
      kenslerIsAlive <- getPartnerIsAlive Assets.drAmyKenslerProfessorOfBiology
      blueStory
        $ validateEntry kenslerIsAlive "kensler.alive"
        <> hr
        <> validateEntry (not kenslerIsAlive) "kensler.otherwise"

      unless kenslerIsAlive do
        eachInvestigator (`sufferPhysicalTrauma` 1)

      scoutedTheForkedPass <- getHasRecord TheInvestigatorsScoutedTheForkedPass
      blueStory
        $ validateEntry scoutedTheForkedPass "scoutedTheForkedPass.yes"
        <> hr
        <> validateEntry (not scoutedTheForkedPass) "scoutedTheForkedPass.no"

      danforthIsAlive <- getPartnerIsAlive Assets.danforthBrilliantStudent
      story
        $ i18n "hoursPass"
        <> blueFlavor
          ( validateEntry danforthIsAlive "danforth.alive"
              <> hr
              <> validateEntry (not danforthIsAlive) "danforth.otherwise"
          )

      unless danforthIsAlive do
        eachInvestigator (`sufferMentalTrauma` 1)

      miasmicCrystalRecovered <- hasSupply MiasmicCrystal
      blueStory
        $ validateEntry miasmicCrystalRecovered "miasmicCrystal.recovered"
        <> hr
        <> validateEntry (not miasmicCrystalRecovered) "miasmicCrystal.unrecovered"

      unless miasmicCrystalRecovered do
        whenM hasRemainingFrostTokens $ addChaosToken #frost

      storyWithChooseOneM (i18nWithTitle "proceed") do
        labeled
          "Stay here and study the great door to learn more. You will play both parts of the scenario. Proceed to The Heart of Madness, Part 1."
          $ doStep 1 msg
        labeled
          "There is no time to waste. Pass through the gate! You will skip the first part of the scenario. Skip directly to The Heart of Madness, Part 2."
          $ doStep 2 msg

      pure s
    DoStep n PreScenarioSetup -> do
      pure $ TheHeartOfMadness $ attrs & metaL .~ toJSON n
    Setup -> do
      doStep (toResult @Int attrs.meta) msg
      pure s
    DoStep 1 Setup -> runScenarioSetup TheHeartOfMadness attrs do
      gather Set.TheHeartOfMadness
      gather Set.TheGreatSeal
      gather Set.Miasma
      gather Set.NamelessHorrors
      gather Set.Penguins
      gather Set.Shoggoths
      gather Set.Tekelili
      gather Set.AncientEvils
      gather Set.LockedDoors

      -- Gate starts revealed
      theGateOfYquaa <- place Locations.theGateOfYquaa
      reveal theGateOfYquaa
      moveAllTo attrs theGateOfYquaa

      setAgendaDeck [Agendas.theBeatingHeart, Agendas.theMiasmaBeckons, Agendas.callOfMadness]
      setActDeck [Acts.dormancy, Acts.theGreatSeal]

      placeGroup
        "facility"
        [ Locations.geothermalVent
        , Locations.forsakenTemple
        , Locations.libraryOfKos
        , Locations.protoplasmicPool
        , Locations.hallOfTheSunlessSea
        , Locations.subnauticalSprawl
        , Locations.subnauticalSprawl
        , Locations.vaultedCorridor
        , Locations.vaultedCorridor
        , Locations.vaultedCorridor
        , Locations.glacialGrotto
        , Locations.sculpturedCrypt
        , Locations.undercityAltar
        , Locations.ichorLadenTunnels
        , Locations.limestoneCaverns
        ]

      doStep 11 Setup

      case attrs.difficulty of
        Expert -> placeDoomOnAgenda 2
        Hard -> placeDoomOnAgenda 1
        _ -> pure ()
      addTekeliliDeck
    DoStep 11 Setup -> do
      let
        connectLocations a b = do
          l1 <- selectJust $ LocationWithLabel a
          l2 <- selectJust $ LocationWithLabel b
          push $ AddDirectConnection l1 l2
          push $ AddDirectConnection l2 l1

      connectLocations "facility1" "facility2"
      connectLocations "facility1" "facility3"
      connectLocations "facility1" "facility4"
      connectLocations "facility2" "facility5"
      connectLocations "facility2" "facility14"
      connectLocations "facility3" "facility6"
      connectLocations "facility3" "facility15"
      connectLocations "facility4" "facility5"
      connectLocations "facility4" "facility6"
      connectLocations "facility4" "facility7"
      connectLocations "facility5" "facility8"
      connectLocations "facility5" "facility12"
      connectLocations "facility6" "facility9"
      connectLocations "facility6" "facility13"
      connectLocations "facility7" "facility8"
      connectLocations "facility7" "facility9"
      connectLocations "facility7" "theGateOfYquaa"
      connectLocations "facility8" "facility10"
      connectLocations "facility8" "theGateOfYquaa"
      connectLocations "facility9" "facility11"
      connectLocations "facility9" "theGateOfYquaa"
      connectLocations "facility10" "facility11"
      connectLocations "facility10" "facility12"
      connectLocations "facility10" "theGateOfYquaa"
      connectLocations "facility11" "facility13"
      connectLocations "facility11" "theGateOfYquaa"
      connectLocations "facility12" "facility13"
      connectLocations "facility12" "facility14"
      connectLocations "facility13" "facility15"
      connectLocations "facility14" "facility15"
      pure s
    DoStep 2 Setup -> runScenarioSetup TheHeartOfMadness attrs do
      gather Set.TheHeartOfMadness
      gather Set.AgentsOfTheUnknown
      gather Set.Miasma
      gather Set.NamelessHorrors
      gather Set.Penguins
      gather Set.StirringInTheDeep
      gather Set.Tekelili
      gather Set.AncientEvils
      gather Set.ChillingCold
      gather Set.StrikingFear
      pure ()
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ n -> do
      case token.face of
        Tablet | isEasyStandard attrs -> drawTekelili iid Tablet 1
        ElderThing | n >= 3 -> placeDoomOnAgendaAndCheckAdvance 1
        _ -> pure ()
      pure s
    ResolveChaosToken _ Tablet iid | isHardExpert attrs -> do
      drawTekelili iid Tablet 1
      pure s
    _ -> TheHeartOfMadness <$> liftRunMessage msg attrs
