module Arkham.Types.Scenario.Scenarios.TheUnspeakableOath
  ( TheUnspeakableOath(..)
  , theUnspeakableOath
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Location.Cards qualified as Locations
import Arkham.Types.Card
import Arkham.Types.Card.PlayerCard
import Arkham.Types.Classes
import Arkham.Types.Difficulty
import Arkham.Types.EncounterSet qualified as EncounterSet
import Arkham.Types.Helpers
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Scenario.Attrs
import Arkham.Types.Scenario.Helpers
import Arkham.Types.Scenario.Runner
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.Trait hiding (Cultist)

newtype TheUnspeakableOath = TheUnspeakableOath ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theUnspeakableOath :: Difficulty -> TheUnspeakableOath
theUnspeakableOath difficulty =
  TheUnspeakableOath
    $ baseAttrs
        "03159"
        "The Unspeakable Oath"
        [Agendas.lockedInside, Agendas.torturousDescent, Agendas.hisDomain]
        [ Acts.arkhamAsylum
        , Acts.theReallyBadOnesV1
        , Acts.theReallyBadOnesV2
        , Acts.planningTheEscape
        , Acts.noAsylum
        ]
        difficulty
    & locationLayoutL
    ?~ [ ".       .        garden                        .                             ."
       , ".       .        yard                          .                             ."
       , "kitchen messHall asylumHallsWesternPatientWing asylumHallsEasternPatientWing infirmary"
       , ".       .        .                             basementHall                  ."
       ]

instance HasRecord env TheUnspeakableOath where
  hasRecord _ _ = pure False
  hasRecordSet _ _ = pure []
  hasRecordCount _ _ = pure 0

instance
  ( HasTokenValue env InvestigatorId
  , HasCount Shroud env LocationId
  , HasCount HorrorCount env InvestigatorId
  , HasId LocationId env InvestigatorId
  )
  => HasTokenValue env TheUnspeakableOath where
  getTokenValue (TheUnspeakableOath attrs) iid = \case
    Skull -> pure $ if isEasyStandard attrs
      then TokenValue Skull (NegativeModifier 1)
      else TokenValue Skull NoModifier
    Cultist -> do
      horror <- unHorrorCount <$> getCount iid
      pure $ TokenValue Cultist (NegativeModifier horror)
    Tablet -> do
      lid <- getId @LocationId iid
      shroud <- unShroud <$> getCount lid
      pure $ TokenValue Tablet (NegativeModifier shroud)
    ElderThing -> pure $ TokenValue ElderThing ZeroModifier
    otherFace -> getTokenValue attrs iid otherFace

standaloneTokens :: [TokenFace]
standaloneTokens =
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

instance ScenarioRunner env => RunMessage env TheUnspeakableOath where
  runMessage msg s@(TheUnspeakableOath attrs) = case msg of
    SetTokensForScenario -> do
      -- TODO: move to helper since consistent
      standalone <- getIsStandalone
      randomToken <- sample (Cultist :| [Tablet, ElderThing])
      s <$ if standalone
        then push (SetTokens $ standaloneTokens <> [randomToken, randomToken])
        else pure ()
    Setup -> do
      gatheredCards <- buildEncounterDeck
        [ EncounterSet.TheUnspeakableOath
        , EncounterSet.HastursGift
        , EncounterSet.InhabitantsOfCarcosa
        , EncounterSet.Delusions
        , EncounterSet.DecayAndFilth
        , EncounterSet.AgentsOfHastur
        ]

      westernPatientWing <- genCard =<< sample
        (Locations.asylumHallsWesternPatientWing_168
        :| [Locations.asylumHallsWesternPatientWing_169]
        )

      easternPatientWing <- genCard =<< sample
        (Locations.asylumHallsEasternPatientWing_170
        :| [Locations.asylumHallsEasternPatientWing_171]
        )

      messHall <- genCard Locations.messHall
      kitchen <- genCard Locations.kitchen
      yard <- genCard Locations.yard
      garden <- genCard Locations.garden
      infirmary <- genCard Locations.infirmary
      basementHall <- genCard Locations.basementHall

      setAsideCards' <- traverse
        genCard
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
        setAsideCards =
          map EncounterCard (monsters <> lunatics) <> setAsideCards'
      investigatorIds <- getInvestigatorIds
      courageMessages <- concat <$> for
        investigatorIds
        \iid -> do
          deck <- map unDeckCard <$> getList iid
          case deck of
            (x : _) -> do
              courageProxy <- genPlayerCard Assets.courage
              let
                courage = PlayerCard
                  (courageProxy { pcOriginalCardCode = toCardCode x })
              pure
                [ DrawCards iid 1 False
                , InitiatePlayCardAs iid (toCardId x) courage [] False
                ]
            _ -> error "empty investigator deck"
      let
        spawnMessages = map
          (\iid -> chooseOne
            iid
            [ TargetLabel
                (LocationTarget $ toLocationId location)
                [MoveTo (toSource attrs) iid (toLocationId location)]
            | location <- [westernPatientWing, easternPatientWing]
            ]
          )
          investigatorIds
      pushAllEnd
        $ [ SetEncounterDeck encounterDeck
          , AddAgenda "03160"
          , AddAct "03163"
          , PlaceLocation westernPatientWing
          , SetLocationLabel
            (toLocationId westernPatientWing)
            "asylumHallsWesternPatientWing"
          , PlaceLocation easternPatientWing
          , SetLocationLabel
            (toLocationId easternPatientWing)
            "asylumHallsEasternPatientWing"
          , PlaceLocation messHall
          , PlaceLocation kitchen
          , PlaceLocation yard
          , PlaceLocation garden
          , PlaceLocation infirmary
          , PlaceLocation basementHall
          ]
        <> spawnMessages
        <> courageMessages
      TheUnspeakableOath
        <$> runMessage msg (attrs & setAsideCardsL .~ setAsideCards)
    ResolveToken _ tokenFace iid -> case tokenFace of
      Skull -> s <$ when (isHardExpert attrs) (push $ DrawAnotherToken iid)
      ElderThing -> do
        monsters <- getSetAsideCardsMatching
          (CardWithType EnemyType <> CardWithTrait Monster)
        case monsters of
          [] -> s <$ push FailSkillTest
          (x : xs) -> do
            monster <- sample (x :| xs)
            s <$ push
              (chooseOne
                iid
                [ Label
                  "Randomly choose an enemy from among the set-aside Monster enemies and place it beneath the act deck without looking at it"
                  [PlaceUnderneath ActDeckTarget [monster]]
                , Label "This test automatically fails" [FailSkillTest]
                ]
              )
      _ -> pure s
    FailedSkillTest iid _ _ (TokenTarget token) _ _ -> do
      case tokenFace token of
        Skull -> do
          monsters <- getSetAsideCardsMatching
            (CardWithType EnemyType <> CardWithTrait Monster)
          case monsters of
            [] -> pure ()
            (x : xs) -> do
              monster <- sample (x :| xs)
              push (PlaceUnderneath ActDeckTarget [monster])
        Cultist -> push
          $ InvestigatorAssignDamage iid (TokenSource token) DamageAny 0 1
        Tablet ->
          push $ InvestigatorAssignDamage iid (TokenSource token) DamageAny 0 1
        _ -> pure ()
      pure s
    _ -> TheUnspeakableOath <$> runMessage msg attrs
