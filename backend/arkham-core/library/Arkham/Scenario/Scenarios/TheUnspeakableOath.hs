module Arkham.Scenario.Scenarios.TheUnspeakableOath (
  TheUnspeakableOath (..),
  theUnspeakableOath,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.CampaignStep
import Arkham.Campaigns.ThePathToCarcosa.Helpers
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Classes.HasGame
import Arkham.Cost
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers
import Arkham.Helpers.Investigator
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Message
import Arkham.Movement
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.TheUnspeakableOath.Story
import Arkham.Trait hiding (Cultist, Expert)
import Arkham.Window qualified as Window

newtype TheUnspeakableOath = TheUnspeakableOath ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theUnspeakableOath :: Difficulty -> TheUnspeakableOath
theUnspeakableOath difficulty =
  scenarioWith
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
    (decksL .~ mapFromList [(LunaticsDeck, []), (MonstersDeck, [])])

instance HasChaosTokenValue TheUnspeakableOath where
  getChaosTokenValue iid chaosTokenFace (TheUnspeakableOath attrs) = case chaosTokenFace of
    Skull ->
      pure
        $ if isEasyStandard attrs
          then ChaosTokenValue Skull (NegativeModifier 1)
          else ChaosTokenValue Skull NoModifier
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

investigatorDefeat :: HasGame m => m [Message]
investigatorDefeat = do
  investigatorIds <- allInvestigatorIds
  players <- allPlayers
  defeatedInvestigatorIds <- select DefeatedInvestigator
  if null defeatedInvestigatorIds
    then pure []
    else
      pure
        $ story players defeat
        : map DrivenInsane defeatedInvestigatorIds
          <> [ GameOver
             | null
                ( setFromList @(Set InvestigatorId) investigatorIds
                    `difference` setFromList @(Set InvestigatorId)
                      defeatedInvestigatorIds
                )
             ]

instance RunMessage TheUnspeakableOath where
  runMessage msg s@(TheUnspeakableOath attrs) = case msg of
    SetChaosTokensForScenario -> do
      whenM getIsStandalone $ do
        randomToken <- sample (Cultist :| [Tablet, ElderThing])
        push (SetChaosTokens $ standaloneChaosTokens <> [randomToken, randomToken])
      pure s
    Setup -> do
      gatheredCards <-
        buildEncounterDeck
          [ EncounterSet.TheUnspeakableOath
          , EncounterSet.HastursGift
          , EncounterSet.InhabitantsOfCarcosa
          , EncounterSet.Delusions
          , EncounterSet.DecayAndFilth
          , EncounterSet.AgentsOfHastur
          ]

      westernPatientWing <-
        sample
          ( Locations.asylumHallsWesternPatientWing_168
              :| [Locations.asylumHallsWesternPatientWing_169]
          )

      easternPatientWing <-
        sample
          ( Locations.asylumHallsEasternPatientWing_170
              :| [Locations.asylumHallsEasternPatientWing_171]
          )

      placeOtherLocations <-
        traverse
          placeLocationCard_
          [ Locations.messHall
          , Locations.kitchen
          , Locations.yard
          , Locations.garden
          , Locations.infirmary
          , Locations.basementHall
          ]

      setAsideCards <-
        genCards
          [ Assets.danielChesterfield
          , Locations.patientConfinementDrearyCell
          , Locations.patientConfinementDanielsCell
          , Locations.patientConfinementOccupiedCell
          , Locations.patientConfinementFamiliarCell
          ]
      let
        (monsters, deck') =
          partition (`cardMatch` CardWithTrait Monster) (unDeck gatheredCards)
        (lunatics, deck'') =
          partition (`cardMatch` CardWithTrait Lunatic) deck'
        encounterDeck = Deck deck''
      investigatorIds <- allInvestigatorIds
      constanceInterviewed <- interviewed Assets.constanceDumaine
      courageMessages <-
        if constanceInterviewed
          then
            concat <$> for investigatorIds \iid -> do
              deck <- fieldMap InvestigatorDeck unDeck iid
              case deck of
                (x : _) -> do
                  courageProxy <- genPlayerCard Assets.courage
                  let
                    courage =
                      PlayerCard
                        (courageProxy {pcOriginalCardCode = toCardCode x})
                  pure
                    [ drawCards iid attrs 1
                    , InitiatePlayCardAs
                        iid
                        (PlayerCard x)
                        courage
                        []
                        LeaveChosenCard
                        NoPayment
                        (Window.defaultWindows iid)
                        False
                    ]
                _ -> error "empty investigator deck"
          else pure []
      theFollowersOfTheSignHaveFoundTheWayForward <-
        getHasRecord
          TheFollowersOfTheSignHaveFoundTheWayForward

      (easternPatientWingId, placeEasternPatientWing) <-
        placeLocationCard
          easternPatientWing
      (westernPatientWingId, placeWesternPatientWing) <-
        placeLocationCard
          westernPatientWing

      investigatorPlayers <- allInvestigatorPlayers
      let
        players = map snd investigatorPlayers
        spawnMessages =
          map
            ( \(iid, player) ->
                chooseOne
                  player
                  [ TargetLabel
                    (LocationTarget location)
                    [MoveTo $ move (toSource attrs) iid location]
                  | location <- [westernPatientWingId, easternPatientWingId]
                  ]
            )
            investigatorPlayers
        intro1Or2 =
          if theFollowersOfTheSignHaveFoundTheWayForward
            then intro1
            else intro2
        tokenToAdd = case scenarioDifficulty attrs of
          Easy -> MinusTwo
          Standard -> MinusThree
          Hard -> MinusFour
          Expert -> MinusFive

      pushAll
        $ [story players intro1Or2, story players intro3]
        <> [ story players constancesInformation
           | constanceInterviewed
           ]
        <> courageMessages
        <> [ SetEncounterDeck encounterDeck
           , SetAgendaDeck
           , SetActDeck
           , placeWesternPatientWing
           , SetLocationLabel
              westernPatientWingId
              "asylumHallsWesternPatientWing"
           , placeEasternPatientWing
           , SetLocationLabel
              easternPatientWingId
              "asylumHallsEasternPatientWing"
           ]
        <> placeOtherLocations
        <> [AddChaosToken tokenToAdd]
        <> spawnMessages

      tookTheOnyxClasp <- getHasRecord YouTookTheOnyxClasp
      let
        theReallyBadOnes =
          if tookTheOnyxClasp
            then Acts.theReallyBadOnesV1
            else Acts.theReallyBadOnesV2

      acts <-
        genCards
          [ Acts.arkhamAsylum
          , theReallyBadOnes
          , Acts.planningTheEscape
          , Acts.noAsylum
          ]
      agendas <-
        genCards
          [Agendas.lockedInside, Agendas.torturousDescent, Agendas.hisDomain]

      TheUnspeakableOath
        <$> runMessage
          msg
          ( attrs
              & (setAsideCardsL .~ setAsideCards)
              & (decksL . at LunaticsDeck ?~ map EncounterCard lunatics)
              & (decksL . at MonstersDeck ?~ map EncounterCard monsters)
              & (actStackL . at 1 ?~ acts)
              & (agendaStackL . at 1 ?~ agendas)
          )
    ResolveChaosToken _ Skull iid -> do
      pushWhen (isHardExpert attrs) (DrawAnotherChaosToken iid)
      pure s
    ResolveChaosToken _ ElderThing iid -> do
      player <- getPlayer iid
      monsters <- getSetAsideCardsMatching (CardWithType EnemyType <> CardWithTrait Monster)
      case monsters of
        [] -> push FailSkillTest
        (x : xs) -> do
          monster <- sample (x :| xs)
          push
            $ chooseOne
              player
              [ Label
                  "Randomly choose an enemy from among the set-aside Monster enemies and place it beneath the act deck without looking at it"
                  [PlaceUnderneath ActDeckTarget [monster]]
              , Label "This test automatically fails" [FailSkillTest]
              ]
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        Skull -> do
          monsters <-
            getSetAsideCardsMatching
              (CardWithType EnemyType <> CardWithTrait Monster)
          case monsters of
            [] -> pure ()
            (x : xs) -> do
              monster <- sample (x :| xs)
              push (PlaceUnderneath ActDeckTarget [monster])
        Cultist | isHardExpert attrs -> do
          push $ InvestigatorAssignDamage iid (ChaosTokenSource token) DamageAny 0 1
        Tablet | isHardExpert attrs -> do
          push $ InvestigatorAssignDamage iid (ChaosTokenSource token) DamageAny 0 1
        _ -> pure ()
      pure s
    ScenarioResolution NoResolution -> do
      push R1
      pure . TheUnspeakableOath $ attrs & inResolutionL .~ True
    ScenarioResolution (Resolution n) -> do
      msgs <- investigatorDefeat
      lead <- getLeadPlayer
      investigatorIds <- allInvestigatorIds
      players <- allPlayers
      gainXp <- toGainXp attrs getXp
      constanceSlain <-
        selectOne
          (VictoryDisplayCardMatch $ cardIs Enemies.constanceDumaine)
      let danielWasAlly = toCardCode Assets.danielChesterfield `elem` scenarioResignedCardCodes attrs
      danielWasEnemy <- selectAny (enemyIs Enemies.danielChesterfield)

      let
        interludeResult
          | danielWasAlly = DanielSurvived
          | danielWasEnemy = DanielWasPossessed
          | otherwise = DanielDidNotSurvive

      let
        updateSlain =
          [ recordSetInsert VIPsSlain [toCardCode constance]
          | constance <- maybeToList constanceSlain
          ]
        removeTokens =
          [ RemoveAllChaosTokens Cultist
          , RemoveAllChaosTokens Tablet
          , RemoveAllChaosTokens ElderThing
          ]

      case n of
        1 -> do
          youTookTheOnyxClasp <- getHasRecord YouTookTheOnyxClasp
          claspMessages <-
            if youTookTheOnyxClasp
              then do
                pure
                  [ RemoveCampaignCard Assets.claspOfBlackOnyx
                  , chooseOne
                      lead
                      [ TargetLabel
                        (InvestigatorTarget iid)
                        [AddCampaignCardToDeck iid Assets.claspOfBlackOnyx]
                      | iid <- investigatorIds
                      ]
                  ]
              else pure []
          pushAll
            $ msgs
            <> [ story players resolution1
               , Record TheKingClaimedItsVictims
               ]
            <> gainXp
            <> claspMessages
            <> updateSlain
            <> removeTokens
            <> [AddChaosToken Cultist, AddChaosToken Cultist]
            <> [EndOfGame Nothing]
        2 ->
          pushAll
            $ msgs
            <> [story players resolution2]
            <> [Record TheInvestigatorsWereAttackedAsTheyEscapedTheAsylum]
            <> [SufferTrauma iid 1 0 | iid <- investigatorIds]
            <> gainXp
            <> updateSlain
            <> removeTokens
            <> [AddChaosToken Tablet, AddChaosToken Tablet]
            <> [EndOfGame (Just $ InterludeStep 2 (Just interludeResult))]
        3 ->
          pushAll
            $ msgs
            <> [story players resolution3]
            <> [Record TheInvestigatorsEscapedTheAsylum]
            <> gainXp
            <> updateSlain
            <> removeTokens
            <> [AddChaosToken ElderThing, AddChaosToken ElderThing]
            <> [EndOfGame (Just $ InterludeStep 2 (Just interludeResult))]
        _ -> error "invalid resolution"
      pure s
    _ -> TheUnspeakableOath <$> runMessage msg attrs
