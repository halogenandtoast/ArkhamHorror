module Arkham.Scenario.Scenarios.TheUnspeakableOath (theUnspeakableOath, TheUnspeakableOath (..), setupTheUnspeakableOath) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignStep
import Arkham.Campaigns.ThePathToCarcosa.Import
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Cost
import Arkham.Difficulty
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Exception
import Arkham.Helpers
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Investigator
import Arkham.Helpers.Query
import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Message.Lifted.Move
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Helpers hiding (forceAddCampaignCardToDeckChoice)
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.TheUnspeakableOath.Helpers
import Arkham.Trait hiding (Cultist, ElderThing, Expert)
import Arkham.Treachery.Cards qualified as Treacheries
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
          monster <- sample (x :| xs)
          chooseOneM iid do
            labeled
              "Randomly choose an enemy from among the set-aside Monster enemies and place it beneath the act deck without looking at it"
              $ placeUnderneath ActDeckTarget [monster]
            labeled "This test automatically fails" failSkillTest
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        Skull -> case attrs.deck MonstersDeck of
          [] -> pure ()
          (x : xs) -> do
            monster <- sample (x :| xs)
            placeUnderneath ActDeckTarget [monster]
        Cultist | isHardExpert attrs -> assignHorror iid Cultist 1
        Tablet | isHardExpert attrs -> assignHorror iid Tablet 1
        _ -> pure ()
      pure s
    ScenarioResolution r -> scope "resolutions" do
      defeated <- select DefeatedInvestigator
      investigators <- allInvestigators
      unless (null defeated) do
        flavor $ scope "defeated" $ h "title" >> p "body"
        for_ defeated drivenInsane
      if length defeated == length investigators
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
          resolutionWithXp "resolution1" $ allGainXp' attrs
          record TheKingClaimedItsVictims
          whenHasRecord YouTookTheOnyxClasp do
            withOwner Assets.claspOfBlackOnyx \owner -> do
              removeCampaignCard Assets.claspOfBlackOnyx
              investigators <- allInvestigators
              forceAddCampaignCardToDeckChoice
                (filter (/= owner) investigators)
                DoNotShuffleIn
                Assets.claspOfBlackOnyx
          updateSlain
          replaceSymbolTokens Cultist
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
    _ -> TheUnspeakableOath <$> liftRunMessage msg attrs
