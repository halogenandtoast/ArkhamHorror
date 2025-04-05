module Arkham.Scenario.Scenarios.TheHeartOfMadnessPart1 (theHeartOfMadnessPart1) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.EdgeOfTheEarth.CampaignSteps (pattern TheHeartOfMadnessPart2)
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Campaigns.EdgeOfTheEarth.Key
import Arkham.Campaigns.EdgeOfTheEarth.Supplies
import Arkham.EncounterSet qualified as Set
import Arkham.Exception
import Arkham.FlavorText
import Arkham.Helpers.ChaosBag
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.SkillTest
import Arkham.Helpers.Text
import Arkham.Helpers.Xp
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Message.Lifted.Move (moveAllTo)
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.TheHeartOfMadness.Helpers

newtype TheHeartOfMadnessPart1 = TheHeartOfMadnessPart1 ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHeartOfMadnessPart1 :: Difficulty -> TheHeartOfMadnessPart1
theHeartOfMadnessPart1 difficulty =
  scenarioWith
    TheHeartOfMadnessPart1
    "08648a"
    "The Heart of Madness"
    difficulty
    theHeartOfMadnessLayout
    (referenceL .~ "08648")

instance HasModifiersFor TheHeartOfMadnessPart1 where
  getModifiersFor (TheHeartOfMadnessPart1 _a) = do
    getSkillTestInvestigator >>= traverse_ \iid -> do
      whenM (sealAtLocationOf iid) do
        modifySelect Cultist (ChaosTokenFaceIs #cultist) [ChaosTokenFaceModifier [#frost]]

instance HasChaosTokenValue TheHeartOfMadnessPart1 where
  getChaosTokenValue = getChaosTokenValueFromScenario

instance RunMessage TheHeartOfMadnessPart1 where
  runMessage msg s@(TheHeartOfMadnessPart1 attrs) = runQueueT $ scenarioI18n 1 $ case msg of
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
          "Stay here and study the great door to learn more. You will play both parts of the scenario. Proceed to _The Heart of Madness, Part 1._"
          nothing
        labeled
          "There is no time to waste. Pass through the gate! You will skip the first part of the scenario. Skip directly to _The Heart of Madness, Part 2_."
          $ endOfScenarioThen TheHeartOfMadnessPart2

      pure s
    Setup -> runScenarioSetup TheHeartOfMadnessPart1 attrs do
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

      doStep 2 Setup

      case attrs.difficulty of
        Expert -> placeDoomOnAgenda 2
        Hard -> placeDoomOnAgenda 1
        _ -> pure ()
      addTekeliliDeck
    DoStep 2 Setup -> do
      connectAllLocations
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ n -> do
      case token.face of
        Tablet | isEasyStandard attrs -> drawTekelili iid Tablet 1
        ElderThing | n >= 3 -> placeDoomOnAgendaAndCheckAdvance 1
        _ -> pure ()
      pure s
    ResolveChaosToken _ Tablet iid | isHardExpert attrs -> do
      drawTekelili iid Tablet 1
      pure s
    ScenarioResolution resolution -> scope "resolutions" do
      case resolution of
        NoResolution -> do
          story $ i18nWithTitle "noResolution"
          push R2
        Resolution 1 -> do
          locationSeals <-
            count (.active) . concatMap toList <$> selectField LocationSeals LocationWithAnyActiveSeal
          investigatorSeals <-
            count (.active) . concatMap toList <$> selectField InvestigatorSeals InvestigatorWithAnyActiveSeal
          base <- allGainXpWithBonus' attrs $ toBonus "bonus" (locationSeals + investigatorSeals)
          placedSeals <-
            fieldMap LocationSeals (filter (.active) . toList)
              =<< selectJust (locationIs Locations.theGateOfYquaa)
          unless (null placedSeals) $ recordSetInsert SealsPlaced (map toJSON placedSeals)
          recoveredSeals <- concatMap toList <$> selectField InvestigatorSeals Anyone
          unless (null recoveredSeals) $ recordSetInsert SealsRecovered (map toJSON recoveredSeals)
          story
            $ withVars ["xp" .= base]
            $ i18nWithTitle "resolution1"
          endOfScenario
        Resolution 2 -> do
          locationSeals <-
            count (.active) . concatMap toList <$> selectField LocationSeals LocationWithAnyActiveSeal
          investigatorSeals <-
            count (.active) . concatMap toList <$> selectField InvestigatorSeals InvestigatorWithAnyActiveSeal
          placedSeals <-
            fieldMap LocationSeals (filter (.active) . toList)
              =<< selectJust (locationIs Locations.theGateOfYquaa)
          unless (null placedSeals) $ recordSetInsert SealsPlaced (map toJSON placedSeals)
          recoveredSeals <- concatMap toList <$> selectField InvestigatorSeals Anyone
          unless (null recoveredSeals) $ recordSetInsert SealsRecovered (map toJSON recoveredSeals)
          base <- allGainXpWithBonus' attrs $ toBonus "bonus" (locationSeals + investigatorSeals)
          story
            $ withVars ["xp" .= base]
            $ i18nWithTitle "resolution2"
          endOfScenario
        Resolution 3 -> do
          story $ i18nWithTitle "resolution3"
          record TheSealWasUsedImproperly
          eachInvestigator (kill attrs)
          gameOver
        _ -> throwIO $ UnknownResolution resolution
      pure s
    _ -> TheHeartOfMadnessPart1 <$> liftRunMessage msg attrs
