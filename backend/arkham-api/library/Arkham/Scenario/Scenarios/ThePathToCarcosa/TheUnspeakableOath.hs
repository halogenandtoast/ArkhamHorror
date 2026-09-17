module Arkham.Scenario.Scenarios.ThePathToCarcosa.TheUnspeakableOath (theUnspeakableOath, TheUnspeakableOath (..), setupTheUnspeakableOath) where

import Arkham.Act.CardDefs.ReturnToThePathToCarcosa.ReturnToTheUnspeakableOath qualified as Acts
import Arkham.Act.CardDefs.ThePathToCarcosa.TheUnspeakableOath qualified as Acts
import Arkham.Agenda.CardDefs.ThePathToCarcosa.TheUnspeakableOath qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignStep
import Arkham.Campaigns.ThePathToCarcosa.Import
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Cost
import Arkham.Difficulty
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.CardDefs.ReturnToThePathToCarcosa.ReturnToTheUnspeakableOath qualified as Enemies
import Arkham.Enemy.CardDefs.ThePathToCarcosa.TheLastKing qualified as Enemies
import Arkham.Enemy.CardDefs.ThePathToCarcosa.TheUnspeakableOath qualified as Enemies
import Arkham.Exception
import Arkham.Helpers
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Investigator
import Arkham.Helpers.Query
import Arkham.Helpers.Xp (getInitialVictory)
import Arkham.I18n
import Arkham.Id (InvestigatorId, getPlayer)
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.CardDefs.ThePathToCarcosa.TheUnspeakableOath qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Message.Lifted.Move
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Helpers hiding (forceAddCampaignCardToDeckChoice)
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.ThePathToCarcosa.TheUnspeakableOath.Helpers
import Arkham.Trait hiding (Cultist, ElderThing, Expert)
import Arkham.Treachery.CardDefs.ReturnToThePathToCarcosa.ReturnToTheUnspeakableOath qualified as Treacheries
import Arkham.UltimatumsAndBoons
import Arkham.Window qualified as Window

newtype TheUnspeakableOath = TheUnspeakableOath ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theUnspeakableOath :: Difficulty -> TheUnspeakableOath
theUnspeakableOath difficulty =
  scenario
    TheUnspeakableOath
    "03159"
    "The Unspeakable Oath"
    difficulty
    [ ".       .       .        .        garden                        garden                        .                             .                             .                   ."
    , ".       .       .        .        yard                          yard                          .                             .                             .                   ."
    , "kitchen kitchen messHall messHall asylumHallsWesternPatientWing asylumHallsWesternPatientWing asylumHallsEasternPatientWing asylumHallsEasternPatientWing infirmary           infirmary"
    , ".       .       .        .        patientConfinement1           patientConfinement1           basementHall                  basementHall                  patientConfinement2 patientConfinement2"
    , ".       .       .        .        .                             patientConfinement3           patientConfinement3           patientConfinement4           patientConfinement4 ."
    ]

instance HasChaosTokenValue TheUnspeakableOath where
  getChaosTokenValue iid chaosTokenFace (TheUnspeakableOath attrs) = case chaosTokenFace of
    Skull ->
      pure
        $ ChaosTokenValue Skull
        $ if isEasyStandard attrs then NegativeModifier 1 else NoModifier
    Cultist -> do
      horror <- field InvestigatorHorror iid
      pure $ ChaosTokenValue Cultist (NegativeModifier horror)
    Tablet -> do
      lid <- getJustLocation iid
      shroud <- fieldJust LocationShroud lid
      pure $ ChaosTokenValue Tablet (NegativeModifier shroud)
    ElderThing -> pure $ ChaosTokenValue ElderThing ZeroModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

standaloneChaosTokens :: [ChaosTokenFace]
standaloneChaosTokens =
  [ PlusOne
  , Zero
  , Zero
  , MinusOne
  , MinusOne
  , MinusOne
  , MinusTwo
  , MinusTwo
  , MinusThree
  , MinusThree
  , MinusFour
  , Skull
  , Skull
  , Skull
  , AutoFail
  , ElderSign
  ]

setupTheUnspeakableOath :: (HasI18n, ReverseQueue m) => ScenarioAttrs -> ScenarioBuilderT m ()
setupTheUnspeakableOath attrs = do
  setup do
    ul do
      li "gatherSets"
      li "monsters"
      li "lunatics"
      li "chooseLocations"
      li "setAside"
      li "placeLocations"
      li "adjustChaosBag"
      li.nested "act2.instructions" do
        li "act2.v1"
        li "act2.v2"
      unscoped $ li "shuffleRemainder"
  whenReturnTo $ gather Set.ReturnToTheUnspeakableOath
  gather Set.TheUnspeakableOath
  gather Set.HastursGift
  gather Set.InhabitantsOfCarcosa
  gather Set.Delusions `orWhenReturnTo` gather Set.MaddeningDelusions
  gather Set.DecayAndFilth `orWhenReturnTo` gather Set.DecayingReality
  gather Set.AgentsOfHastur `orWhenReturnTo` gather Set.HastursEnvoys

  placeAll
    [ Locations.messHall
    , Locations.kitchen
    , Locations.yard
    , Locations.garden
    , Locations.infirmary
    , Locations.basementHall
    ]

  setAside
    [ Assets.danielChesterfield
    , Locations.patientConfinementDrearyCell
    , Locations.patientConfinementDanielsCell
    , Locations.patientConfinementOccupiedCell
    , Locations.patientConfinementFamiliarCell
    ]

  whenReturnTo $ setAside [Enemies.hostOfInsanity, Treacheries.radicalTreatment]

  easternPatientWing <-
    placeLabeled "asylumHallsEasternPatientWing"
      =<< sample2 Locations.asylumHallsEasternPatientWing_170 Locations.asylumHallsEasternPatientWing_171

  westernPatientWing <-
    placeLabeled "asylumHallsWesternPatientWing"
      =<< sample2 Locations.asylumHallsWesternPatientWing_168 Locations.asylumHallsWesternPatientWing_169

  addChaosToken $ case attrs.difficulty of
    Easy -> MinusTwo
    Standard -> MinusThree
    Hard -> MinusFour
    Expert -> MinusFive

  eachInvestigator \iid -> do
    chooseTargetM iid [westernPatientWing, easternPatientWing] $ moveTo_ attrs iid

  theReallyBadOnes <- do
    isReturnTo <- getIsReturnTo
    if not isReturnTo
      then do
        tookTheOnyxClasp <- getHasRecord YouTookTheOnyxClasp
        pure $ if tookTheOnyxClasp then Acts.theReallyBadOnesV1 else Acts.theReallyBadOnesV2
      else do
        v3 <- liftA2 (>=) getConviction getDoubt
        pure $ if v3 then Acts.theReallyBadOnesV3 else Acts.theReallyBadOnesV4

  setActDeck
    [ Acts.arkhamAsylum
    , theReallyBadOnes
    , Acts.planningTheEscape
    , Acts.noAsylum
    ]
  setAgendaDeck [Agendas.lockedInside, Agendas.torturousDescent, Agendas.hisDomain]

  addExtraDeck LunaticsDeck =<< shuffle =<< amongGathered (CardWithTrait Lunatic <> #enemy)
  addExtraDeck MonstersDeck =<< shuffle =<< amongGathered (CardWithTrait Monster <> #enemy)

instance RunMessage TheUnspeakableOath where
  runMessage msg s@(TheUnspeakableOath attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      foundTheWayForward <- getHasRecord TheFollowersOfTheSignHaveFoundTheWayForward
      flavor do
        h "title"
        p.validate foundTheWayForward "foundTheWayForward"
        p.validate (not foundTheWayForward) "didNotFindTheWayForward"
      flavor do
        h "title"
        p $ if foundTheWayForward then "intro1" else "intro2"

      didInterview <- interviewed Assets.constanceDumaine
      flavor do
        h "title"
        p "intro3"
        unscoped (campaignI18n (nameVar Assets.constanceDumaine $ p "checkIfInterviewed"))
        p.right.validate didInterview "proceedToConstancesInformation"
        p.right.validate (not didInterview) "otherwise"

      when didInterview do
        flavor do
          h "title"
          p "constancesInformation"
        eachInvestigator \iid -> do
          deck <- fieldMap InvestigatorDeck unDeck iid
          case deck of
            (x : _) -> do
              let courage = x {pcCardCode = Assets.courage.cardCode}
              replaceCard courage.id (PlayerCard courage)
              obtainCard x
              push
                $ InitiatePlayCard
                  iid
                  (PlayerCard courage)
                  Nothing
                  NoPayment
                  (Window.defaultWindows iid)
                  False
            _ -> error "empty investigator deck"
      pure s
    StandaloneSetup -> do
      randomToken <- sample (Cultist :| [Tablet, ElderThing])
      setChaosTokens $ standaloneChaosTokens <> [randomToken, randomToken]
      pure s
    Setup -> runScenarioSetup TheUnspeakableOath attrs $ setupTheUnspeakableOath attrs
    ResolveChaosToken _ Skull iid -> do
      when (isHardExpert attrs) $ drawAnotherChaosToken iid
      pure s
    ResolveChaosToken _ ElderThing iid -> do
      case attrs.deck MonstersDeck of
        [] -> failSkillTest
        (x : xs) -> do
          monster <- setFacedown True =<< sample (x :| xs)
          chooseOneM iid do
            labeled "placeMonsterUnderActDeck"
              $ placeUnderneath ActDeckTarget [monster]
            labeled "thisTestAutomaticallyFails" failSkillTest
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        Skull -> case attrs.deck MonstersDeck of
          [] -> pure ()
          (x : xs) -> do
            monster <- setFacedown True =<< sample (x :| xs)
            placeUnderneath ActDeckTarget [monster]
        Cultist | isHardExpert attrs -> assignHorror iid Cultist 1
        Tablet | isHardExpert attrs -> assignHorror iid Tablet 1
        _ -> pure ()
      pure s
    ScenarioResolution r -> scope "resolutions" do
      defeated <- select DefeatedInvestigator
      investigators <- allInvestigators
      unless (null defeated) do
        flavor $ scope "defeated" $ setTitle "title" >> p "body"
        for_ defeated drivenInsane
      -- "If there are not enough investigators to continue the campaign, the campaign is
      -- over and the players lose." That pool is every investigator the players can still
      -- build a deck for, not the ones who just sat down, so losing the whole table does
      -- not end the campaign: Resolution 1 seats the replacements instead. Ultimatum of
      -- Survival is the exception -- it eliminates the player along with the investigator,
      -- so there is nobody left to come back with.
      survival <- hasUltimatum UltimatumOfSurvival
      if survival && length defeated == length investigators
        then gameOver
        else case r of
          NoResolution -> do_ R1
          _ -> do_ msg
      pure s
    Do (ScenarioResolution r) -> scope "resolutions" do
      constanceSlain <- selectOne (VictoryDisplayCardMatch $ basic $ cardIs Enemies.constanceDumaine)
      let danielWasAlly = toCardCode Assets.danielChesterfield `elem` attrs.resignedCardCodes
      danielWasEnemy <- selectAny (enemyIs Enemies.danielChesterfield)

      let
        interludeResult
          | danielWasAlly = DanielSurvived
          | danielWasEnemy = DanielWasPossessed
          | otherwise = DanielDidNotSurvive

      let
        updateSlain = for_ constanceSlain \constance -> recordSetInsert VIPsSlain [toCardCode constance]
        replaceSymbolTokens symbol = do
          for_ [Cultist, Tablet, ElderThing] removeAllChaosTokens
          twice $ addChaosToken symbol

      case r of
        Resolution 1 -> do
          -- The experience is the victory display's total, which does not depend on who is
          -- sitting at the table, so the resolution can be read out with the right number
          -- before anything else happens. Earning it is a separate, later bullet -- see the
          -- DoStep below, which runs once the table is the one continuing the campaign.
          resolutionWithXp "resolution1" getInitialVictory
          record TheKingClaimedItsVictims
          updateSlain
          replaceSymbolTokens Cultist

          -- The clasp's bearer is normally one of the investigators just driven insane, so
          -- take the card off them here, while getOwner can still see them: once their
          -- player picks a replacement below they are gone from the game, and the clasp
          -- would stay owned by an investigator ReloadDecks never deals in again. Who it
          -- goes to is decided after that, so the owner rides along to the DoStep.
          mClasp <- runMaybeT do
            guard =<< lift (getHasRecord YouTookTheOnyxClasp)
            clasp <- MaybeT $ fetchCardMaybe Assets.claspOfBlackOnyx
            mOwner <- lift $ getOwner Assets.claspOfBlackOnyx
            lift do
              removeCampaignCard Assets.claspOfBlackOnyx
              for_ mOwner (`removeCardFromDeckForCampaign` clasp)
            pure (clasp, mOwner)

          -- "Each player whose investigator has been driven insane must choose a new
          -- investigator from the pool of available investigators." This has to happen
          -- inside the resolution rather than at the usual between-scenario upgrade window,
          -- because the two bullets after it -- the clasp and the experience -- belong to
          -- those new investigators. Ultimatum of Survival keeps those players out,
          -- matching the campaign's own killed/insane handling.
          survival <- hasUltimatum UltimatumOfSurvival
          insane <- if survival then pure [] else select InsaneInvestigator
          unless (null insane) do
            push . Msg.chooseUpgradeDecks =<< traverse getPlayer insane

          -- Queued after the deck window, so both remaining bullets see the investigators
          -- who are actually continuing the campaign.
          doStep 1 (ScenarioSpecific "resolution1" (toJSON mClasp))
          endOfScenario
        Resolution 2 -> do
          resolutionWithXp "resolution2" $ allGainXp' attrs
          record TheInvestigatorsWereAttackedAsTheyEscapedTheAsylum
          eachInvestigator (`sufferPhysicalTrauma` 1)
          updateSlain
          replaceSymbolTokens Tablet
          endOfScenarioThen (InterludeStep 2 (Just interludeResult))
        Resolution 3 -> do
          resolutionWithXp "resolution3" $ allGainXp' attrs
          record TheInvestigatorsEscapedTheAsylum
          updateSlain
          replaceSymbolTokens ElderThing
          endOfScenarioThen (InterludeStep 2 (Just interludeResult))
        _ -> throw $ UnknownResolution r
      pure s
    DoStep 1 (ScenarioSpecific "resolution1" v) -> do
      -- Resolution 1's last two bullets, run once every replacement investigator is seated.
      let mClasp = toResult v :: Maybe (Card, Maybe InvestigatorId)

      -- "Check Campaign Log. If you took the onyx clasp, choose a new investigator to take
      -- the clasp. That investigator must include the Clasp of Black Onyx weakness in his
      -- or her deck."
      for_ mClasp \(clasp, mOwner) -> do
        -- Not allInvestigators: the scenario's turn order still lists the investigators who
        -- started it, so a replacement seated moments ago would be filtered straight out of
        -- it. Anyone still in the campaign is a candidate, resigned survivors included.
        investigators <-
          select $ IncludeEliminated (not_ KilledInvestigator <> not_ InsaneInvestigator)
        -- The previous bearer has usually been replaced by now, so this only bites when
        -- Resolution 1 was reached with them still around (they resigned); fall back to the
        -- whole table rather than dropping the clasp out of the campaign.
        let candidates = case filter (\iid -> Just iid /= mOwner) investigators of
              [] -> investigators
              others -> others
        unless (null candidates)
          $ forceAddCampaignCardToDeckChoice candidates DoNotShuffleIn clasp

      -- "Each investigator earns experience equal to the Victory X value of each card in
      -- the victory display." A replacement investigator builds their deck with no
      -- experience and then earns this, so it has to land on them rather than on the
      -- investigator they took over from -- otherwise the campaign log credits the
      -- scenario's experience to someone who is no longer in the campaign.
      allGainXp attrs
      pure s
    _ -> TheUnspeakableOath <$> liftRunMessage msg attrs
