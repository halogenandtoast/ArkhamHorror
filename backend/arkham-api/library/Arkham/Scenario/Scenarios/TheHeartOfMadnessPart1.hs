module Arkham.Scenario.Scenarios.TheHeartOfMadnessPart1 (theHeartOfMadnessPart1) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign.Option
import Arkham.CampaignLog
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Campaigns.EdgeOfTheEarth.Key
import Arkham.EncounterSet qualified as Set
import Arkham.Exception
import Arkham.FlavorText
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Query (allInvestigators)
import Arkham.Helpers.SkillTest
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
      isStandalone <- getIsStandalone
      when (not isStandalone || attrs.hasOption PerformIntro) do
        story $ i18nWithTitle "part1Intro"

      when (not isStandalone || attrs.hasOption IncludePartners) do
        eachInvestigator (`forInvestigator` PreScenarioSetup)

      if attrs.hasOption IncludePartners
        then do
          let addPartner partner = standaloneCampaignLogL . partnersL . at partner.cardCode ?~ CampaignLogPartner 0 0 Safe
          pure $ TheHeartOfMadnessPart1 $ foldl' (flip addPartner) attrs expeditionTeam
        else pure s
    StandaloneSetup -> do
      setChaosTokens (#elderthing : #elderthing : chaosBagContents attrs.difficulty)
      pure s
    ForInvestigator iid PreScenarioSetup -> do
      partners <- getRemainingPartners
      unless (null partners) do
        chooseOneM iid do
          questionLabeled "Choose a partner for this scenario"
          labeled "Do not take a partner" nothing
          for_ partners \partner -> do
            inPlay <- selectAny $ assetIs partner.cardCode
            unless inPlay do
              cardLabeled partner.cardCode $ handleTarget iid ScenarioSource (CardCodeTarget partner.cardCode)
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
    ScenarioResolution r -> scope "resolutions" do
      case r of
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
        _ -> throwIO $ UnknownResolution r
      pure s
    HandleOption option -> do
      investigators <- allInvestigators
      whenM getIsStandalone $ do
        case option of
          AddGreenSoapstone -> forceAddCampaignCardToDeckChoice investigators ShuffleIn Assets.greenSoapstoneJinxedIdol
          AddWoodenSledge -> forceAddCampaignCardToDeckChoice investigators ShuffleIn Assets.woodenSledge
          AddDynamite -> forceAddCampaignCardToDeckChoice investigators ShuffleIn Assets.dynamite
          AddMiasmicCrystal -> forceAddCampaignCardToDeckChoice investigators ShuffleIn Assets.miasmicCrystalStrangeEvidence
          AddMineralSpecimen -> forceAddCampaignCardToDeckChoice investigators ShuffleIn Assets.mineralSpecimen
          AddSmallRadio -> forceAddCampaignCardToDeckChoice investigators ShuffleIn Assets.smallRadio
          AddSpareParts -> forceAddCampaignCardToDeckChoice investigators ShuffleIn Assets.spareParts
          PerformIntro -> pure ()
          IncludePartners -> pure ()
          _ -> error $ "Unhandled option: " <> show option
      pure s
    _ -> TheHeartOfMadnessPart1 <$> liftRunMessage msg attrs
