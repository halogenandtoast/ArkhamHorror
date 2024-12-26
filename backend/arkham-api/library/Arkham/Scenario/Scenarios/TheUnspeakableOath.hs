module Arkham.Scenario.Scenarios.TheUnspeakableOath (TheUnspeakableOath (..), theUnspeakableOath) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.CampaignStep
import Arkham.Campaigns.ThePathToCarcosa.Helpers
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Cost
import Arkham.Difficulty
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers
import Arkham.Helpers.Investigator
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Helpers hiding (defeated, forceAddCampaignCardToDeckChoice, recordSetInsert)
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.TheUnspeakableOath.Story
import Arkham.Strategy
import Arkham.Trait hiding (Cultist, ElderThing, Expert)
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

investigatorDefeat :: ReverseQueue m => m ()
investigatorDefeat = do
  defeated <- select DefeatedInvestigator
  unless (null defeated) do
    story defeat
    for_ defeated drivenInsane
    investigators <- allInvestigators
    when (length defeated == length investigators) gameOver

instance RunMessage TheUnspeakableOath where
  runMessage msg s@(TheUnspeakableOath attrs) = runQueueT $ case msg of
    PreScenarioSetup -> do
      foundTheWayForward <- getHasRecord TheFollowersOfTheSignHaveFoundTheWayForward
      story $ if foundTheWayForward then intro1 else intro2
      story intro3
      whenInterviewed Assets.constanceDumaine do
        story constancesInformation
        eachInvestigator \iid -> do
          deck <- fieldMap InvestigatorDeck unDeck iid
          case deck of
            (x : _) -> do
              courageProxy <- genPlayerCard Assets.courage
              let courage = PlayerCard (courageProxy {pcOriginalCardCode = toCardCode x})
              forcedDrawCards iid attrs 1
              push
                $ InitiatePlayCardAs
                  iid
                  (PlayerCard x)
                  courage
                  []
                  LeaveChosenCard
                  NoPayment
                  (Window.defaultWindows iid)
                  False
            _ -> error "empty investigator deck"
      pure s
    StandaloneSetup -> do
      randomToken <- sample (Cultist :| [Tablet, ElderThing])
      setChaosTokens $ standaloneChaosTokens <> [randomToken, randomToken]
      pure s
    Setup -> runScenarioSetup TheUnspeakableOath attrs do
      gather Set.TheUnspeakableOath
      gather Set.HastursGift
      gather Set.InhabitantsOfCarcosa
      gather Set.Delusions
      gather Set.DecayAndFilth
      gather Set.AgentsOfHastur

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

      tookTheOnyxClasp <- getHasRecord YouTookTheOnyxClasp
      let theReallyBadOnes = if tookTheOnyxClasp then Acts.theReallyBadOnesV1 else Acts.theReallyBadOnesV2

      setActDeck
        [ Acts.arkhamAsylum
        , theReallyBadOnes
        , Acts.planningTheEscape
        , Acts.noAsylum
        ]
      setAgendaDeck [Agendas.lockedInside, Agendas.torturousDescent, Agendas.hisDomain]

      addExtraDeck LunaticsDeck =<< shuffle =<< amongGathered (CardWithTrait Lunatic <> #enemy)
      addExtraDeck MonstersDeck =<< shuffle =<< amongGathered (CardWithTrait Monster <> #enemy)
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
    ScenarioResolution NoResolution -> do
      push R1
      pure s
    ScenarioResolution (Resolution n) -> do
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
          removeAllChaosTokens Cultist
          removeAllChaosTokens Tablet
          removeAllChaosTokens ElderThing
          replicateM_ 2 $ addChaosToken symbol

      investigatorDefeat
      case n of
        1 -> do
          story resolution1
          record TheKingClaimedItsVictims
          allGainXp attrs
          whenHasRecord YouTookTheOnyxClasp do
            withOwner Assets.claspOfBlackOnyx \owner -> do
              removeCampaignCard Assets.claspOfBlackOnyx
              investigators <- allInvestigators
              forceAddCampaignCardToDeckChoice (filter (/= owner) investigators) Assets.claspOfBlackOnyx
          updateSlain
          replaceSymbolTokens Cultist
          endOfScenario
        2 -> do
          story resolution2
          record TheInvestigatorsWereAttackedAsTheyEscapedTheAsylum
          eachInvestigator (`sufferPhysicalTrauma` 1)
          allGainXp attrs
          updateSlain
          replaceSymbolTokens Tablet
          endOfScenarioThen (InterludeStep 2 (Just interludeResult))
        3 -> do
          story resolution3
          record TheInvestigatorsEscapedTheAsylum
          allGainXp attrs
          updateSlain
          replaceSymbolTokens ElderThing
          endOfScenarioThen (InterludeStep 2 (Just interludeResult))
        _ -> error "invalid resolution"
      pure s
    _ -> TheUnspeakableOath <$> liftRunMessage msg attrs
