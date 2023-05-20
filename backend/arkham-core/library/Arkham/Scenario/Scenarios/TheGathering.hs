module Arkham.Scenario.Scenarios.TheGathering where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Exception
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.TheGathering.Story
import Arkham.Token
import Arkham.Trait qualified as Trait

newtype TheGathering = TheGathering ScenarioAttrs
  deriving stock (Generic)
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

theGathering :: Difficulty -> TheGathering
theGathering difficulty =
  scenario
    TheGathering
    "01104"
    "The Gathering"
    difficulty
    ["   .   attic   .     ", " study hallway parlor", "   .   cellar  .     "]

instance HasTokenValue TheGathering where
  getTokenValue iid tokenFace (TheGathering attrs) = case tokenFace of
    Skull -> do
      ghoulCount <-
        selectCount $
          EnemyAt (LocationWithInvestigator $ InvestigatorWithId iid)
            <> EnemyWithTrait Trait.Ghoul
      pure $ toTokenValue attrs Skull ghoulCount 2
    Cultist ->
      pure $
        TokenValue
          Cultist
          (if isEasyStandard attrs then NegativeModifier 1 else NoModifier)
    Tablet -> pure $ toTokenValue attrs Tablet 2 4
    otherFace -> getTokenValue iid otherFace attrs

theGatheringAgendaDeck :: [CardDef]
theGatheringAgendaDeck =
  [Agendas.whatsGoingOn, Agendas.riseOfTheGhouls, Agendas.theyreGettingOut]

instance RunMessage TheGathering where
  runMessage msg s@(TheGathering attrs) = case msg of
    Setup -> do
      investigatorIds <- allInvestigatorIds
      encounterDeck <-
        buildEncounterDeckExcluding
          [Enemies.ghoulPriest]
          [ EncounterSet.TheGathering
          , EncounterSet.Rats
          , EncounterSet.Ghouls
          , EncounterSet.StrikingFear
          , EncounterSet.AncientEvils
          , EncounterSet.ChillingCold
          ]
      (studyId, placeStudy) <- placeLocationCard Locations.study

      pushAll
        [ SetEncounterDeck encounterDeck
        , SetAgendaDeck
        , SetActDeck
        , placeStudy
        , RevealLocation Nothing studyId
        , MoveAllTo (toSource attrs) studyId
        , story investigatorIds theGatheringIntro
        ]

      setAsideCards <-
        genCards
          [ Enemies.ghoulPriest
          , Assets.litaChantler
          , Locations.hallway
          , Locations.attic
          , Locations.cellar
          , Locations.parlor
          ]

      agendas <- genCards theGatheringAgendaDeck
      acts <- genCards [Acts.trapped, Acts.theBarrier, Acts.whatHaveYouDone]

      TheGathering
        <$> runMessage
          msg
          ( attrs
              & (setAsideCardsL .~ setAsideCards)
              & (actStackL . at 1 ?~ acts)
              & (agendaStackL . at 1 ?~ agendas)
          )
    ResolveToken _ Cultist iid ->
      s <$ when (isHardExpert attrs) (push $ DrawAnotherToken iid)
    ResolveToken _ Tablet iid -> do
      ghoulCount <-
        selectCount $
          EnemyAt (LocationWithInvestigator $ InvestigatorWithId iid)
            <> EnemyWithTrait Trait.Ghoul
      s
        <$ when
          (ghoulCount > 0)
          ( push $
              InvestigatorAssignDamage
                iid
                (TokenEffectSource Tablet)
                DamageAny
                1
                (if isEasyStandard attrs then 0 else 1)
          )
    FailedSkillTest iid _ _ (TokenTarget token) _ _ -> do
      case tokenFace token of
        Skull | isHardExpert attrs -> do
          push $
            FindAndDrawEncounterCard
              iid
              (CardWithType EnemyType <> CardWithTrait Trait.Ghoul)
              True
        Cultist ->
          push $
            InvestigatorAssignDamage
              iid
              (TokenSource token)
              DamageAny
              0
              (if isEasyStandard attrs then 1 else 2)
        _ -> pure ()
      pure s
    ScenarioResolution resolution -> do
      lead <- getLead
      iids <- allInvestigatorIds
      xp <- getXpWithBonus 2
      let
        xpGain = [GainXP iid (toSource attrs) n | (iid, n) <- xp]
        chooseToAddLita =
          chooseOne
            lead
            [ Label
                "Add Lita Chantler to your deck"
                [AddCampaignCardToDeck lead Assets.litaChantler]
            , Label "Do not add Lita Chantler to your deck" []
            ]
      case resolution of
        NoResolution ->
          pushAll $
            [ story iids noResolution
            , Record YourHouseIsStillStanding
            , Record GhoulPriestIsStillAlive
            , chooseToAddLita
            ]
              <> xpGain
              <> [EndOfGame Nothing]
        Resolution 1 ->
          pushAll $
            [ story iids resolution1
            , Record YourHouseHasBurnedToTheGround
            , chooseToAddLita
            , SufferTrauma lead 0 1
            ]
              <> xpGain
              <> [EndOfGame Nothing]
        Resolution 2 ->
          -- TODO: Combine gainXP and bonus so modifiers work
          pushAll $
            [ story iids resolution2
            , Record YourHouseIsStillStanding
            , GainXP lead (toSource attrs) 1
            ]
              <> xpGain
              <> [EndOfGame Nothing]
        Resolution 3 ->
          pushAll
            -- TODO: missing rules
            -- \* kill non-resigned investigators
            -- \* end campaign if none left
            -- \* handle new investigators
            -- \* handle lead being killed
            [ story iids resolution3
            , Record LitaWasForcedToFindOthersToHelpHerCause
            , Record YourHouseIsStillStanding
            , Record GhoulPriestIsStillAlive
            , chooseToAddLita
            , EndOfGame Nothing
            ]
        other -> throwIO $ UnknownResolution other
      pure s
    _ -> TheGathering <$> runMessage msg attrs
