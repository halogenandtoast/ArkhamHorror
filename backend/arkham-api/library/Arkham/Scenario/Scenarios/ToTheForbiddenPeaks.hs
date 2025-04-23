module Arkham.Scenario.Scenarios.ToTheForbiddenPeaks (toTheForbiddenPeaks) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign.Option
import Arkham.CampaignLog
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Campaigns.EdgeOfTheEarth.Key
import Arkham.Campaigns.EdgeOfTheEarth.Supplies
import Arkham.Card
import Arkham.Direction
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Exception
import Arkham.Field
import Arkham.FlavorText
import Arkham.Helpers.ChaosBag
import Arkham.Helpers.Location
import Arkham.Helpers.Query (allInvestigators, getLead)
import Arkham.Helpers.Text
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Message.Lifted.Move
import Arkham.Message.Lifted.Placement qualified as P
import Arkham.Modifier
import Arkham.Name (toTitle)
import Arkham.Placement
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.ToTheForbiddenPeaks.Helpers
import Arkham.Trait (Trait (Expedition))
import Arkham.Trait qualified as Trait
import Arkham.Xp

newtype ToTheForbiddenPeaks = ToTheForbiddenPeaks ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- Note: The Campaign Guide refers to it as level 0, but it is easier for us to
-- start with level 1 since we can use the existing helper
toTheForbiddenPeaks :: Difficulty -> ToTheForbiddenPeaks
toTheForbiddenPeaks difficulty =
  scenario
    ToTheForbiddenPeaks
    "08596"
    "To the Forbidden Peaks"
    difficulty
    [ ".      .      .      .      .      theSummit"
    , ".      .      .      .      level5 theSummit"
    , ".      .      .      level4 level5 ."
    , ".      .      level3 level4 .      ."
    , ".      level2 level3 .      .      ."
    , "level1 level2 .      .      .      ."
    , "level1 .      .      .      .      ."
    ]

instance HasChaosTokenValue ToTheForbiddenPeaks where
  getChaosTokenValue iid tokenFace (ToTheForbiddenPeaks attrs) = case tokenFace of
    Skull -> do
      row <-
        fromMaybe 0 <$> runMaybeT do
          loc <- MaybeT $ getLocationOf iid
          pos <- MaybeT $ field LocationPosition loc
          pure $ positionRow pos
      pure $ toChaosTokenValue attrs Skull row (row + 2)
    Cultist -> pure $ ChaosTokenValue Cultist (NegativeModifier 1)
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 4
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 4 5
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage ToTheForbiddenPeaks where
  runMessage msg s@(ToTheForbiddenPeaks attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      isStandalone <- getIsStandalone
      when (not isStandalone || attrs.hasOption PerformIntro) (doStep 0 msg)
      if attrs.hasOption IncludePartners
        then do
          eachInvestigator (`forInvestigator` PreScenarioSetup)
          let addPartner partner = standaloneCampaignLogL . partnersL . at partner.cardCode ?~ CampaignLogPartner 0 0 Safe
          pure $ ToTheForbiddenPeaks $ foldl' (flip addPartner) attrs expeditionTeam
        else pure s
    DoStep 0 PreScenarioSetup -> do
      story $ i18nWithTitle "intro1"

      eliyahIsAlive <- getPartnerIsAlive Assets.eliyahAshevakDogHandler
      woodenSledgeRecovered <- hasSupply WoodenSledge
      blueStory
        $ validateEntry (eliyahIsAlive && woodenSledgeRecovered) "eliyah.alive"
        <> hr
        <> validateEntry (not $ eliyahIsAlive && woodenSledgeRecovered) "eliyah.otherwise"

      unless (eliyahIsAlive && woodenSledgeRecovered) do
        whenM hasRemainingFrostTokens $ addChaosToken #frost

      claypoolIsAlive <- getPartnerIsAlive Assets.averyClaypoolAntarcticGuide
      blueStory
        $ validateEntry claypoolIsAlive "claypool.alive"
        <> hr
        <> validateEntry (not claypoolIsAlive) "claypool.otherwise"

      lead <- getLead

      unless claypoolIsAlive do
        chooseOneM lead do
          whenM hasRemainingFrostTokens do
            labeled "Add 1 {frost} token to the chaos bag" $ addChaosToken #frost
          labeled "Each investigator suffers 1 physical trauma." $ eachInvestigator (`sufferPhysicalTrauma` 1)

      takadaIsAlive <- getPartnerIsAlive Assets.takadaHirokoAeroplaneMechanic
      blueStory
        $ validateEntry takadaIsAlive "takada.alive"
        <> hr
        <> validateEntry (not takadaIsAlive) "takada.otherwise"

      unless takadaIsAlive do
        chooseOneM lead do
          whenM hasRemainingFrostTokens do
            labeled "Add 1 {frost} token to the chaos bag" $ addChaosToken #frost
          labeled "Each investigator suffers 1 mental trauma." $ eachInvestigator (`sufferMentalTrauma` 1)

      scoutedTheMountainPass <- getHasRecord TheInvestigatorsScoutedTheMountainPass
      blueStory
        $ validateEntry scoutedTheMountainPass "theMountainPass.scouted"
        <> hr
        <> validateEntry (not scoutedTheMountainPass) "theMountainPass.otherwise"

      story $ i18nWithTitle "intro2"
      eachInvestigator (`forInvestigator` PreScenarioSetup)
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
    HandleTargetChoice iid (isSource attrs -> True) (CardCodeTarget cardCode) -> do
      for_ (lookupCardDef cardCode) \def -> do
        card <- genCard def
        assetId <- createAssetAt card (InPlayArea iid)
        partner <- getPartner cardCode
        pushWhen (partner.damage > 0) $ Msg.PlaceDamage CampaignSource (toTarget assetId) partner.damage
        pushWhen (partner.horror > 0) $ Msg.PlaceHorror CampaignSource (toTarget assetId) partner.horror
      pure s
    StandaloneSetup -> do
      setChaosTokens $ chaosBagContents attrs.difficulty
      pure s
    Setup -> runScenarioSetup ToTheForbiddenPeaks attrs do
      gather Set.ToTheForbiddenPeaks
      gather Set.DeadlyWeather
      gather Set.ElderThings
      gather Set.HazardsOfAntarctica
      gather Set.NamelessHorrors
      gather Set.Tekelili

      setAgendaDeck [Agendas.forbiddenPeaks, Agendas.terrorDescends]
      setActDeck [Acts.ascendTheMountain]

      mountainSides <-
        placeGroupCapture "level"
          =<< fmap (map toCardDef . take 5)
          . shuffle
          =<< amongGathered (#location <> CardWithTitle "Mountainside")

      theSummit <- place Locations.theSummit

      for_ (withIndex (mountainSides <> [theSummit])) \(m, loc) ->
        push $ UpdateLocation loc $ Update LocationPosition (Just $ Pos 0 m)

      for_ (nonEmpty mountainSides) \(level0 :| rest) -> do
        whenHasRecord TheInvestigatorsScoutedTheMountainPass do
          setupModifier ScenarioSource level0 ReduceStartingCluesByHalf
        startAt level0
        zipWithM_
          (\a b -> push $ Msg.PlacedLocationDirection a LeftOf b)
          (level0 : rest)
          (rest <> [theSummit])
        getSomeRecordSetJSON SuppliesRecovered >>= traverse_ \case
          SpareParts -> assetAt_ Assets.spareParts level0
          GreenSoapstone -> assetAt_ Assets.greenSoapstoneJinxedIdol level0
          SmallRadio -> assetAt_ Assets.smallRadio level0
          MineralSpecimen -> assetAt_ Assets.mineralSpecimen level0
          Dynamite -> assetAt_ Assets.dynamite level0
          WoodenSledge -> assetAt_ Assets.woodenSledge level0
          MiasmicCrystal -> assetAt_ Assets.miasmicCrystalStrangeEvidence level0

      addChaosToken ElderThing
      setAside [Enemies.terrorOfTheStarsGuardianOfForbiddenPeaks]

      case attrs.difficulty of
        Expert -> placeDoomOnAgenda 2
        Hard -> placeDoomOnAgenda 1
        _ -> pure ()

      addTekeliliDeck
    ResolveChaosToken _ Cultist iid -> do
      void $ runMaybeT do
        guard (isHardExpert attrs)
        loc <- MaybeT $ getLocationOf iid
        Pos _ y <- MaybeT $ field LocationPosition loc
        loc' <- MaybeT $ selectOne (LocationInRow (y - 1))
        lift $ afterSkillTest $ moveTo (toSource Cultist) iid loc'

      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        Cultist | isEasyStandard attrs -> void $ runMaybeT do
          guard $ isHardExpert attrs
          loc <- MaybeT $ getLocationOf iid
          Pos _ y <- MaybeT $ field LocationPosition loc
          loc' <- MaybeT $ selectOne (LocationInRow (y - 1))
          lift $ afterSkillTest $ moveTo (toSource Cultist) iid loc'
        Tablet -> withLocationOf iid \loc -> do
          assets <- select $ assetControlledBy iid <> withTrait Expedition
          chooseTargetM iid assets (`P.place` loc)
        ElderThing -> withLocationOf iid \loc -> do
          elderThings <- select $ NearestEnemyTo iid $ withTrait Trait.ElderThing
          chooseTargetM iid elderThings \elderThing -> do
            moveTowardsMatching ElderThing elderThing (LocationWithId loc)
            forTarget elderThing msg
        _ -> pure ()
      pure s
    ForTarget (EnemyTarget eid) (FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _) -> do
      case token.face of
        ElderThing -> whenM (eid <=~> enemyEngagedWith iid) $ initiateEnemyAttack eid ElderThing iid
        _ -> pure ()
      pure s
    ScenarioResolution resolution -> scope "resolutions" do
      case resolution of
        NoResolution -> do
          story $ i18nWithTitle "noResolution"
          push R2
        Resolution 1 -> do
          base <- allGainXp' attrs
          story
            $ withVars ["xp" .= base]
            $ i18nWithTitle "resolution1"
          record TheTeamClimbedToTheSummit
          expeditionAssets <- select $ VictoryDisplayCardMatch $ basic $ withTrait Expedition <> #asset
          investigators <- allInvestigators
          for_ expeditionAssets \card -> do
            addCampaignCardToDeckChoiceWith investigators DoNotShuffleIn card
              $ \iid -> [ReportXp $ XpBreakdown [InvestigatorGainXp iid $ XpDetail XpBonus (toTitle card) 1]]

          let expeditionAssetCardCodes = map toCardCode expeditionAssets
          crossOutRecordSetEntries SuppliesRecovered
            $ map toJSON
            $ filter ((`notElem` expeditionAssetCardCodes) . toCardCode) [GreenSoapstone ..]
          endOfScenario
        Resolution 2 -> do
          base <- allGainXp' attrs
          story
            $ withVars ["xp" .= base]
            $ i18nWithTitle "resolution2"
          record TheTeamFoundAnotherWayThroughTheMountains
          crossOutRecordSetEntries SuppliesRecovered $ map toJSON [GreenSoapstone ..]
          endOfScenario
        _ -> throwIO $ UnknownResolution resolution
      pure s
    HandleOption option -> do
      whenM getIsStandalone do
        case option of
          PerformIntro -> pure ()
          IncludePartners -> pure ()
          _ -> error $ "Unhandled option: " <> show option
      pure s
    _ -> ToTheForbiddenPeaks <$> liftRunMessage msg attrs
