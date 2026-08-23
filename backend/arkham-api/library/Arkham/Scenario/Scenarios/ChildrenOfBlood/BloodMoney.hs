module Arkham.Scenario.Scenarios.ChildrenOfBlood.BloodMoney (bloodMoney) where

import Arkham.Act.CardDefs.ChildrenOfBlood.BloodMoney qualified as Acts
import Arkham.Agenda.CardDefs.ChildrenOfBlood.BloodMoney qualified as Agendas
import Arkham.Asset.Cards.ChildrenOfBlood qualified as Assets
import Arkham.Calculation
import Arkham.Campaigns.ChildrenOfBlood.Key
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.CardDefs.ChildrenOfBlood.AgentsOfZburamoarte qualified as Enemies
import Arkham.Enemy.CardDefs.ChildrenOfBlood.BloodMoney qualified as Enemies
import Arkham.Enemy.CardDefs.ChildrenOfBlood.SanguineSecrets qualified as Enemies
import Arkham.Enemy.Types (Field (EnemySealedChaosTokens, EnemyTokens))
import Arkham.Exception
import Arkham.ForMovement
import Arkham.Helpers.Campaign (getCampaignStoryCards)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Location (getConnectedMoveLocations)
import Arkham.Helpers.Query (
  allInvestigators,
  getPlayerCount,
  getSetAsideCardsMatching,
  selectAssetController,
 )
import Arkham.Helpers.SkillTest (isEvadeWith, isFightWith)
import Arkham.Helpers.Xp
import Arkham.Id
import Arkham.Investigator.Types (Field (InvestigatorHorror))
import Arkham.Location.CardDefs.ChildrenOfBlood.BloodMoney qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.SkillTest.Base
import Arkham.Message.Lifted.Log
import Arkham.Message.Lifted.Move
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Types (Field (ScenarioCardsUnderScenarioReference))
import Arkham.ScenarioLogKey
import Arkham.Scenarios.ChildrenOfBlood.BloodMoney.Helpers
import Arkham.Token
import Arkham.Trait (Trait (Monster))
import Arkham.Trait qualified as Trait
import Arkham.Treachery.CardDefs.ChildrenOfBlood.BloodMoney qualified as Treacheries
import Arkham.Treachery.CardDefs.ChildrenOfBlood.Infected qualified as Treacheries

newtype BloodMoney = BloodMoney ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bloodMoney :: Difficulty -> BloodMoney
bloodMoney difficulty =
  scenario
    BloodMoney
    "13068"
    "Blood Money"
    difficulty
    [ ".        .        .        triangle triangle moon     moon     ."
    , "hourglass hourglass t       t        square   square   diamond  diamond"
    , ".        .        .        circle   circle   .        .        ."
    ]

instance HasChaosTokenValue BloodMoney where
  getChaosTokenValue iid tokenFace (BloodMoney attrs) = case tokenFace of
    Skull -> do
      n <- sealedBloodCount iid
      pure $ toChaosTokenValue attrs Skull (n + 1) (n + 2)
    Cultist -> pure $ toChaosTokenValue attrs Cultist 2 3
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 4
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 4 5
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage BloodMoney where
  runMessage msg s@(BloodMoney attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      story $ i18nWithTitle "intro"
      pure s
    ResolveChaosToken _ Cultist _ | isHardExpert attrs -> do
      whenM isMonsterAttackOrEvade $ afterSkillTestQuiet $ addChaosToken #blood
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        Cultist -> when (isEasyStandard attrs) do
          whenM isMonsterAttackOrEvade $ addChaosToken #blood
        Tablet -> whenM (selectAny $ SealedOnInvestigator (InvestigatorWithId iid) #blood) do
          push $ InvestigatorPlaceCluesOnLocation iid ScenarioSource 1
        ElderThing -> do
          nearest <- select $ NearestEnemyTo iid (ExhaustedEnemy <> EnemyWithTrait Monster)
          chooseOrRunOneM iid $ targets nearest ready
        _ -> pure ()
      pure s
    ScenarioSpecific "codexOn" v -> scope "codex" do
      let (iid :: InvestigatorId, source :: Source, _ :: Int, eid :: EnemyId) = toResult v
      entry "partyGuest"
      -- "until you reveal a token with a symbol that isn't sealed on an enemy"
      requestChaosTokens iid (ProxySource source (EnemySource eid)) 1
      pure s
    RequestedChaosTokens (ProxySource source (EnemySource eid)) (Just iid) [token] -> scope "codex" do
      sealedFaces <- map (.face) <$> selectAgg id EnemySealedChaosTokens AnyEnemy
      if not token.face.isSymbol || token.face `elem` sealedFaces
        then requestChaosTokens iid (ProxySource source (EnemySource eid)) 1
        else do
          -- Priscilla soaks the seal whenever she is in play
          mPriscilla <- selectOne $ enemyIs Enemies.priscillaThomas
          let sealOn e = sealChaosToken iid e token
          let sealPreferringPriscilla = sealOn (fromMaybe eid mPriscilla)
          case token.face of
            Skull -> do
              entry "partyGuestSkull"
              automaticallyEvadeEnemy iid eid
              sealPreferringPriscilla
            Cultist -> do
              entry "partyGuestCultist"
              toDiscardBy iid source eid
              cultists <- getSetAsideCardsMatching (#enemy <> CardWithTrait Trait.Cultist)
              for_ (nonEmpty cultists) \xs -> do
                card <- sample xs
                -- SpawnEngagedWith overrides the enemy's own spawn instruction;
                -- creating it Unplaced and engaging afterwards placed it twice.
                spawned <- createEnemy card iid
                sealChaosToken iid (fromMaybe spawned mPriscilla) token
            Tablet -> do
              entry "partyGuestTablet"
              automaticallyEvadeEnemy iid eid
              sealOn eid
              drawCards iid source 1
            ElderThing -> do
              entry "partyGuestElderThing"
              automaticallyEvadeEnemy iid eid
              sealOn eid
              ls <- getConnectedMoveLocations iid source
              chooseOrRunOneM iid $ withI18n do
                targets ls $ moveTo source iid
                unscoped skip_
            BloodToken -> do
              entry "partyGuestBlood"
              automaticallyEvadeEnemy iid eid
              sealPreferringPriscilla
            AutoFail -> do
              entry "partyGuestAutoFail"
              initiateEnemyAttack eid source iid
              afterEnemyAttack eid do
                disengageFromAll eid
                connected <- select $ ConnectedTo ForMovement (locationWithEnemy eid)
                empties <- filterM (selectNone . investigatorAt) connected
                let ls = if null empties then connected else empties
                chooseOrRunOneM iid $ targets ls $ enemyMoveTo source eid
            ElderSign -> do
              entry "partyGuestElderSign"
              automaticallyEvadeEnemy iid eid
              gainClues iid source 1
              whenJustM (selectOne $ assetIs Assets.detectiveReynoldsInOverHisHead) \reynolds ->
                whenJustM (selectAssetController reynolds) \owner -> gainClues owner source 1
            _ -> pure ()
      pure s
    ScenarioSpecific "codex" v -> scope "codex" do
      let (iid :: InvestigatorId, source :: Source, _ :: Int) = toResult v
      entry "priscillaThomas"
      unlessM (remembered TheInvestigatorsSpokeWithPriscillaThomas) do
        entry "priscillaThomas1"
        remember TheInvestigatorsSpokeWithPriscillaThomas
      doStep 2 (ScenarioSpecific "priscilla" (toJSON (iid, source)))
      pure s
    DoStep 2 (ScenarioSpecific "priscilla" v) -> scope "codex" do
      let (iid :: InvestigatorId, source :: Source) = toResult v
      entry "priscillaThomas2"
      whenJustM (selectOne $ enemyIs Enemies.priscillaThomas) \priscilla -> do
        sealed <- field EnemySealedChaosTokens priscilla
        traverse_ unsealChaosToken sealed
        placeTokens source priscilla Horror (length sealed)
        investigators <- select $ investigatorAt (locationWithInvestigator iid)
        sid <- getRandom
        -- IndexedSource marks this as the codex test, so the horror below is not
        -- also added when Priscilla is fought or evaded the ordinary way.
        chooseOrRunOneM iid $ targets investigators \iid' ->
          chooseBeginSkillTestEdit
            sid
            iid'
            (IndexedSource priscillaCodex source)
            priscilla
            [#willpower, #intellect]
            (Fixed 3)
            (\st -> st {skillTestAction = Just #parley})
      pure s
    -- SkillTestInitiatorTarget only: PassedSkillTest is also pushed once per
    -- skill-test subscriber (committed cards, tokens), which would stack horror.
    PassedSkillTest _ _ (IndexedSource ((== priscillaCodex) -> True) _) SkillTestInitiatorTarget {} _ _ -> do
      whenJustM (selectOne $ enemyIs Enemies.priscillaThomas) \priscilla -> do
        placeTokens ScenarioSource priscilla Horror 1
        -- the placement is still queued, so the threshold has to be read after it
        doStep 4 (ScenarioSpecific "priscilla" Null)
      pure s
    DoStep 4 (ScenarioSpecific "priscilla" _) -> do
      whenJustM (selectOne $ enemyIs Enemies.priscillaThomas) \priscilla -> do
        horror <- fieldMap EnemyTokens (countTokens Horror) priscilla
        when (horror >= 4) $ doStep 3 (ScenarioSpecific "priscilla" Null)
      pure s
    DoStep 3 (ScenarioSpecific "priscilla" _) -> scope "codex" do
      entry "priscillaThomas3"
      whenJustM (selectOne $ enemyIs Enemies.priscillaThomas) removeFromGame
      eachInvestigator \iid -> do
        horror <- field InvestigatorHorror iid
        chooseOneM iid $ withI18n do
          countVar 2 $ labeledValidate' (horror > 0) "healHorror" $ healHorror iid ScenarioSource 2
          countVar 3 $ labeled' "gainResources" $ gainResources iid ScenarioSource 3
      pure s
    Setup -> runScenarioSetup BloodMoney attrs do
      n <- getPlayerCount
      defeatedZburamoarte <- getHasRecord InvestigatorsDefeatedZburamoarte
      killedJulia <- getHasRecord InvestigatorsKilledJuliaStern

      setup $ ul do
        li.nested "gatherSets" do
          li "sanguineSecrets"
        li.validate (n >= 3) "gatherAdditionalSets"
        li.nested "placeLocations" do
          li "foyer"
          li "masterBedroom"
          li.validate (isEasyStandard attrs) "balcony"
          li "startAt"
        li "priscillaThomas"
        li.nested "suspiciousGuests" do
          li.validate (n == 1) "onePlayer"
          li.validate (n == 2) "twoPlayers"
          li.validate (n >= 3) "threeOrFourPlayers"
        li.nested "agendaDeck" do
          li.validate defeatedZburamoarte "feedingFrenzyV2"
          li.validate (not defeatedZburamoarte) "feedingFrenzyV1"
        li "setAside"
        li.nested "howardWilkes" do
          li.validate (attrs.difficulty == Easy) "firstChildOfZburamoarte"
          li.validate (attrs.difficulty == Standard) "tooFarGone"
          li.validate (isHardExpert attrs) "ultimatePredator"
        li.nested.validate (not killedJulia) "juliaStern" do
          li.validate (attrs.difficulty == Easy) "firstVictimOfNewHorizons"
          li.validate (attrs.difficulty == Standard) "outForBlood"
          li.validate (isHardExpert attrs) "childOfVengeance"
        li "addCultist"
        unscoped $ li "shuffleRemainder"
        unscoped $ li "readyToBegin"

      gather Set.BloodMoney
      gather Set.AgentsOfZburamoarte
      gather Set.BloodBlight
      gather Set.Bloodthirst
      gather Set.ChildrenOfBlood
      gather Set.Hunted
      gatherJust Set.SanguineSecrets [Enemies.sanguineCultist, Enemies.bloodCrazedZealot]
      gather Set.ArcaneLock
      when (n >= 3) $ gather Set.Afflicted

      setAgendaDeck
        [ Agendas.partyWithoutAHost
        , if defeatedZburamoarte then Agendas.feedingFrenzyV2 else Agendas.feedingFrenzyV1
        , Agendas.underABloodMoon
        ]
      setActDeck [Acts.dealOrNoDeal, Acts.whereIsWilkes, Acts.bloodbath]

      office <- place Locations.office
      study <- place Locations.study
      diningHall <- place Locations.diningHall
      place_ Locations.kitchen
      startAt =<< place Locations.foyerBoringParty
      setAside [Locations.masterBedroom]
      if isEasyStandard attrs then setAside [Locations.balcony] else removeEvery [Locations.balcony]

      enemyAt_ Enemies.priscillaThomas diningHall
      enemyAt_ Enemies.suspiciousGuest study
      when (n >= 2) $ enemyAt_ Enemies.suspiciousGuest office
      when (n >= 3) $ enemyAt_ Enemies.suspiciousGuest diningHall
      replicateM_ (3 - min 3 n) $ removeOneOfEach [Enemies.suspiciousGuest]

      setAsideEvery $ #enemy <> CardWithTitle "Child of Blood"
      setAsideEvery $ cardIs Enemies.spawnOfZburamoarte
      setAsideEvery $ cardIs Treacheries.sanguineRebirth
      setAsideEvery $ CardFromEncounterSet Set.SanguineSecrets <> #enemy
      setAside [Assets.chosenOfZburamoarteFightingTheHunger]

      setAside
        [ case attrs.difficulty of
            Easy -> Enemies.howardWilkesFirstChildOfZburamoarte
            Standard -> Enemies.howardWilkesTooFarGone
            _other -> Enemies.howardWilkesUltimatePredator
        ]
      removeCards =<< amongGathered (#enemy <> CardWithTitle "Howard Wilkes")

      setAside
        [ case attrs.difficulty of
            Easy -> Enemies.juliaSternFirstVictimOfNewHorizons
            Standard -> Enemies.juliaSternOutForBlood
            _other -> Enemies.juliaSternChildOfVengeance
        | not killedJulia
        ]
      removeCards =<< amongGathered (#enemy <> CardWithTitle "Julia Stern")

      addChaosToken #cultist
    ScenarioResolution r -> scope "resolutions" do
      defeated <- select DefeatedInvestigator
      unless (null defeated) do
        resolution "investigatorDefeat"
        for_ defeated $ kill ScenarioSource

      underneath <- scenarioField ScenarioCardsUnderScenarioReference
      let rescued = count (`cardMatch` (#enemy <> CardWithTrait Trait.Civilian)) underneath

      case r of
        NoResolution -> do
          record InvestigatorsFailedToStopTheChildrenOfBlood
          eachInvestigator \iid -> do
            sufferPhysicalTrauma iid 1
            sufferMentalTrauma iid 1
          resolutionWithXp "noResolution" $ allGainXp' attrs
          doStep 1 msg
          endOfScenario
        Resolution 1 -> do
          record InvestigatorsStoppedTheChildrenOfBlood
          resolutionWithXp "resolution1"
            $ allGainXpWithBonus' attrs (toBonus "rescuedGuests" rescued)
          doStep 1 msg
          endOfScenario
        Resolution 2 -> do
          record InvestigatorsFailedToStopTheChildrenOfBlood
          eachInvestigator \iid -> do
            sufferPhysicalTrauma iid 1
            sufferMentalTrauma iid 1
          resolutionWithXp "resolution2"
            $ allGainXpWithBonus' attrs (toBonus "rescuedGuests" rescued)
          doStep 1 msg
          endOfScenario
        other -> throwIO $ UnknownResolution other
      pure s
    -- shared tail: The Blood Blight bearers, then the campaign reward card
    DoStep 1 (ScenarioResolution _) -> do
      storyCards <- getCampaignStoryCards
      investigators <- allInvestigators
      for_ investigators \iid -> do
        sealed <- selectCount $ SealedOnInvestigator (InvestigatorWithId iid) #blood
        let bearer = any ((== Treacheries.theBloodBlight) . toCardDef) (findWithDefault [] iid storyCards)
        when (sealed >= 2 && not bearer) $ addCampaignCardToDeck iid ShuffleIn Treacheries.theBloodBlight
      pure s
    _ -> BloodMoney <$> liftRunMessage msg attrs

sealedBloodCount :: HasGame m => InvestigatorId -> m Int
sealedBloodCount iid = selectCount $ SealedOnInvestigator (InvestigatorWithId iid) #blood

-- | "during an attack or evasion targeting a {Monster} enemy"
isMonsterAttackOrEvade :: HasGame m => m Bool
isMonsterAttackOrEvade = orM [isFightWith (withTrait Monster), isEvadeWith (withTrait Monster)]

-- | Chapter 2 codex entries render title + body inside the codex frame.
entry :: (HasI18n, ReverseQueue m) => Scope -> m ()
entry x = scope x $ flavor $ setTitle "title" >> compose.codex (h "title" >> p "body")

-- | Index for the Codex B skill test, so its success handler is unambiguous.
priscillaCodex :: Int
priscillaCodex = 2
