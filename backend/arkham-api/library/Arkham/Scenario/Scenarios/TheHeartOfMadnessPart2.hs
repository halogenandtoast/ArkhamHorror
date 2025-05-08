module Arkham.Scenario.Scenarios.TheHeartOfMadnessPart2 (theHeartOfMadnessPart2) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign.Option
import Arkham.CampaignLog
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Campaigns.EdgeOfTheEarth.Key
import Arkham.Campaigns.EdgeOfTheEarth.Seal
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Exception
import Arkham.FlavorText
import Arkham.Helpers.Log ()
import Arkham.Helpers.Query
import Arkham.Helpers.Xp
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Message.Lifted.Move
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.TheHeartOfMadness.Helpers
import Arkham.Treachery.Cards qualified as Treacheries
import Control.Monad.State.Strict (execStateT, modify)

newtype TheHeartOfMadnessPart2 = TheHeartOfMadnessPart2 ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHeartOfMadnessPart2 :: Difficulty -> TheHeartOfMadnessPart2
theHeartOfMadnessPart2 difficulty =
  scenarioWith
    TheHeartOfMadnessPart2
    "08648b"
    "The Heart of Madness"
    difficulty
    theHeartOfMadnessLayout
    (referenceL .~ "08648")

instance HasChaosTokenValue TheHeartOfMadnessPart2 where
  getChaosTokenValue = getChaosTokenValueFromScenario

-- Ensure the inner ring is not all mist pylons
randomizeLocations :: MonadRandom m => m [CardDef]
randomizeLocations = go =<< shuffleM (base <> pylons)
 where
  go ls =
    let innerRing = take 5 (drop 6 ls)
     in if all (`elem` innerRing) pylons
          then go =<< shuffleM ls
          else pure ls
  base =
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
    ]
  pylons =
    [ Locations.mistPylon_174
    , Locations.mistPylon_175
    , Locations.mistPylon_176
    , Locations.mistPylon_177
    , Locations.mistPylon_178
    ]

instance RunMessage TheHeartOfMadnessPart2 where
  runMessage msg s@(TheHeartOfMadnessPart2 attrs) = runQueueT $ scenarioI18n 2 $ case msg of
    StandaloneSetup -> do
      setChaosTokens (#elderthing : #elderthing : chaosBagContents attrs.difficulty)
      lead <- getLead
      chooseOneM lead do
        questionLabeled "The investigators may choose how many seals they have:"
        labeled
          "For an easier experience, three random seals are “Placed” and the other two are “Recovered.”"
          (doStep 1 msg)
        labeled
          "For an average experience, two random seals are “Placed,” one is “Recovered,” and the other two are not used."
          (doStep 2 msg)
        labeled
          "For a harder experience, one random seal is “Placed,” and the other four are not used."
          (doStep 3 msg)
        labeled "For a nightmarish experience, no seals are used." nothing
      pure s
    DoStep n StandaloneSetup -> do
      (placed, recovered) <- case n of
        1 -> splitAt 3 <$> shuffleM [minBound ..]
        2 -> splitAt 2 . take 3 <$> shuffleM [minBound ..]
        3 -> (,[]) . take 1 <$> shuffleM [minBound ..]
        _ -> pure ([], [])
      unless (null placed) $ recordSetInsert SealsPlaced (map (\k -> toJSON $ Seal k True Nothing) placed)
      unless (null recovered)
        $ recordSetInsert SealsRecovered (map (\k -> toJSON $ Seal k False Nothing) recovered)
      pure s
    PreScenarioSetup -> do
      isStandalone <- getIsStandalone
      when (not isStandalone || attrs.hasOption PerformIntro) do
        kenslerAlive <- getPartnerIsAlive Assets.drAmyKenslerProfessorOfBiology
        understandsTheTrueNature <- getHasRecord DrKenslerUnderstandsTheTrueNatureOfTheMiasma
        story $ i18nWithTitle "intro1.main" `addFlavorEntry` ul do
          li.nested "intro1.check" do
            li.validate (kenslerAlive && understandsTheTrueNature) "intro1.intro2"
            li.validate (not $ kenslerAlive && understandsTheTrueNature) "intro1.intro3"

        if kenslerAlive && understandsTheTrueNature
          then doStep 2 msg
          else doStep 3 msg

      when (not isStandalone || attrs.hasOption IncludePartners) do
        eachInvestigator (`forInvestigator` PreScenarioSetup)

      if attrs.hasOption IncludePartners
        then do
          let addPartner partner = standaloneCampaignLogL . partnersL . at partner.cardCode ?~ CampaignLogPartner 0 0 Safe
          pure $ TheHeartOfMadnessPart2 $ foldl' (flip addPartner) attrs expeditionTeam
        else pure s
    DoStep 2 PreScenarioSetup -> do
      story $ i18nWithTitle "intro2"
      record DrKenslerHasAPlan
      pure s
    DoStep 3 PreScenarioSetup -> do
      story $ i18nWithTitle "intro3"
      record TheTruthOfTheMirageEludesYou
      pure s
    ForInvestigator iid PreScenarioSetup -> do
      cannotTakeKensler <- getHasRecord DrKenslerHasAPlan
      let
        filterPartners =
          if cannotTakeKensler
            then
              filter
                ( ( `notElem`
                      [ Assets.drAmyKenslerProfessorOfBiology.cardCode
                      , Assets.drAmyKenslerProfessorOfBiologyResolute.cardCode
                      ]
                  )
                    . toCardCode
                )
            else id
      partners <- filterPartners <$> getRemainingPartners
      unless (null partners) do
        chooseOneM iid do
          questionLabeled "Choose a partner for this scenario"
          labeled "Do not take a partner" nothing
          for_ partners \partner -> do
            inPlay <- selectAny $ assetIs partner.cardCode
            unless inPlay do
              cardLabeled partner.cardCode $ handleTarget iid ScenarioSource (CardCodeTarget partner.cardCode)
      pure s
    Setup -> runScenarioSetup TheHeartOfMadnessPart2 attrs do
      gather Set.TheHeartOfMadness
      gather Set.AgentsOfTheUnknown
      gather Set.Miasma
      gather Set.NamelessHorrors
      gather Set.Penguins
      gather Set.StirringInTheDeep
      gather Set.Tekelili
      gather Set.AncientEvils
      gather Set.ChillingCold
      gatherJust Set.StrikingFear [Treacheries.dissonantVoices, Treacheries.frozenInFear]

      setAgendaDeck [Agendas.theSealWeakens, Agendas.thatWhichHasNoName]
      setActDeck [Acts.collapseThePylons]

      place_ Locations.theGateOfYquaa

      placeGroupExact "facility" =<< randomizeLocations
      doStep 1 msg

      setAside
        $ replicate 15 Enemies.theNamelessMadness
        <> [ Acts.theFinalMirage
           , Locations.titanicRamp_182
           , Locations.titanicRamp_183
           , Locations.titanicRamp_184
           , Locations.titanicRamp_185
           , Locations.hiddenTunnelAWayOut
           ]

      seals <- foldMapM getSomeRecordSetJSON [SealsPlaced, SealsRecovered]
      lead <- getLead
      investigators <- allInvestigators

      for_ seals \seal -> do
        chooseOrRunOneM lead do
          questionLabeled $ "Choose Investigator to take the " <> format seal <> " seal"
          targets investigators (`placeSeal` seal)

      addTekeliliDeck
    DoStep 1 Setup -> do
      connectAllLocations
      lead <- getLead
      ls <-
        select
          $ LocationWithUnrevealedTitle "Ancient Facility"
          <> mapOneOf LocationWithLabel ["facility7", "facility8", "facility9", "facility10", "facility11"]

      chooseTargetM lead ls (\l -> reveal l >> moveAllTo attrs l)

      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          story $ i18nWithTitle "noResolution"
          record TheNamelessMadnessEscaped
          partners <- getRemainingPartners
          for_ partners (`setPartnerStatus` Eliminated)
          eachInvestigator drivenInsane
          gameOver
        Resolution 1 -> do
          xp <- allGainXpWithBonus' attrs $ toBonus "bonus.resolution1" 10
          story $ withVars ["xp" .= xp] $ i18nWithTitle "resolution1"
          record TheNamelessMadnessIsContainedSafelyWithinItsHostForNow
          kensler <- getPartner Assets.drAmyKenslerProfessorOfBiology
          setPartnerStatus kensler TheEntity
          partners <- filter (/= kensler) <$> getRemainingPartners
          investigators <- allInvestigators
          recordSetInsert TheSurvivorsOfTheExpeditionWere
            $ map toCardCode partners
            <> map toCardCode investigators
          endOfScenario
        Resolution 2 -> do
          xp <- allGainXpWithBonus' attrs $ toBonus "bonus.resolution2" 10
          story $ withVars ["xp" .= xp] $ i18nWithTitle "resolution2"
          record TheFacilityWasDestroyed
          partners <- getRemainingPartners
          investigators <- allInvestigators
          recordSetInsert TheSurvivorsOfTheExpeditionWere
            $ map toCardCode partners
            <> map toCardCode investigators
          eachInvestigator (`sufferPhysicalTrauma` 1)
          eachInvestigator (`sufferMentalTrauma` 1)
          endOfScenario
        Resolution 3 -> do
          xp <- allGainXpWithBonus' attrs $ toBonus "bonus.resolution3" 5
          story $ withVars ["xp" .= xp] $ i18nWithTitle "resolution3"
          record TheTeamEscapedTheFacility
          partners <- getRemainingPartners
          investigators <- allInvestigators
          recordSetInsert TheSurvivorsOfTheExpeditionWere
            $ map toCardCode partners
            <> map toCardCode investigators
          eachInvestigator (`sufferPhysicalTrauma` 2)
          eachInvestigator (`sufferMentalTrauma` 2)
          endOfScenario
        _ -> throwIO $ UnknownResolution r
      pure s
    HandleOption option -> do
      investigators <- allInvestigators
      flip execStateT s do
        whenM getIsStandalone do
          case option of
            AddGreenSoapstone -> forceAddCampaignCardToDeckChoice investigators ShuffleIn Assets.greenSoapstoneJinxedIdol
            AddWoodenSledge -> forceAddCampaignCardToDeckChoice investigators ShuffleIn Assets.woodenSledge
            AddDynamite -> forceAddCampaignCardToDeckChoice investigators ShuffleIn Assets.dynamite
            AddMiasmicCrystal -> forceAddCampaignCardToDeckChoice investigators ShuffleIn Assets.miasmicCrystalStrangeEvidence
            AddMineralSpecimen -> forceAddCampaignCardToDeckChoice investigators ShuffleIn Assets.mineralSpecimen
            AddSmallRadio -> forceAddCampaignCardToDeckChoice investigators ShuffleIn Assets.smallRadio
            AddSpareParts -> forceAddCampaignCardToDeckChoice investigators ShuffleIn Assets.spareParts
            PerformIntro -> modify $ overAttrs $ standaloneCampaignLogL . optionsL %~ insertSet IncludePartners
            IncludePartners -> pure ()
            _ -> error $ "Unhandled option: " <> show option
    _ -> TheHeartOfMadnessPart2 <$> liftRunMessage msg attrs
