module Arkham.Scenario.Scenarios.TheHouseAlwaysWins
  ( TheHouseAlwaysWins(..)
  , theHouseAlwaysWins
  ) where

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
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Location.Cards qualified as Locations
import Arkham.Message
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TheHouseAlwaysWins.Story
import Arkham.Source
import Arkham.Target
import Arkham.Token

newtype TheHouseAlwaysWins = TheHouseAlwaysWins ScenarioAttrs
  deriving stock Generic
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

theHouseAlwaysWins :: Difficulty -> TheHouseAlwaysWins
theHouseAlwaysWins difficulty = scenario
  TheHouseAlwaysWins
  "02062"
  "The House Always Wins"
  difficulty
  [ ".           .                .                  backHallDoorway1 ."
  , ".           .                cloverClubCardroom backHallDoorway1 ."
  , "laBellaLuna cloverClubLounge cloverClubCardroom darkenedHall     backHallDoorway2"
  , "laBellaLuna cloverClubLounge cloverClubBar      darkenedHall     backHallDoorway2"
  , ".           .                cloverClubBar      backHallDoorway3 ."
  , ".           .                .                  backHallDoorway3 ."
  ]

instance HasTokenValue TheHouseAlwaysWins where
  getTokenValue iid tokenFace (TheHouseAlwaysWins attrs) = case tokenFace of
    Skull -> pure $ toTokenValue attrs Skull 2 3
    Cultist -> pure $ TokenValue Cultist (NegativeModifier 3)
    Tablet -> pure $ TokenValue Tablet (NegativeModifier 2)
    otherFace -> getTokenValue iid otherFace attrs

instance RunMessage TheHouseAlwaysWins where
  runMessage msg s@(TheHouseAlwaysWins attrs) = case msg of
    Setup -> do
      investigatorIds <- getInvestigatorIds

      encounterDeck <- buildEncounterDeckExcluding
        [Assets.peterClover, Enemies.cloverClubPitBoss]
        [ EncounterSet.TheHouseAlwaysWins
        , EncounterSet.BadLuck
        , EncounterSet.NaomisCrew
        , EncounterSet.Rats
        ]

      cloverClubPitBoss <- genCard Enemies.cloverClubPitBoss
      laBellaLuna <- genCard Locations.laBellaLuna
      cloverClubLounge <- genCard Locations.cloverClubLounge
      cloverClubBar <- genCard Locations.cloverClubBar
      cloverClubCardroom <- genCard Locations.cloverClubCardroom

      let
        laBellaLunaId = toLocationId laBellaLuna
        cloverClubLoungeId = toLocationId cloverClubLounge

      pushAllEnd
        [ SetEncounterDeck encounterDeck
        , SetAgendaDeck
        , SetActDeck
        , PlaceLocation laBellaLuna
        , PlaceLocation cloverClubLounge
        , PlaceLocation cloverClubBar
        , PlaceLocation cloverClubCardroom
        , RevealLocation Nothing laBellaLunaId
        , MoveAllTo (toSource attrs) laBellaLunaId
        , CreateEnemyAt cloverClubPitBoss cloverClubLoungeId Nothing
        , story investigatorIds intro
        ]

      setAsideCards <- traverse
        genCard
        [ Locations.darkenedHall
        , Assets.peterClover
        , Assets.drFrancisMorgan
        , Locations.artGallery
        , Locations.vipArea
        , Locations.backAlley
        ]

      TheHouseAlwaysWins <$> runMessage
        msg
        (attrs
        & (setAsideCardsL .~ setAsideCards)
        & (actStackL
          . at 1
          ?~ [Acts.beginnersLuck, Acts.skinGame, Acts.allIn, Acts.fold]
          )
        & (agendaStackL
          . at 1
          ?~ [ Agendas.theCloverClub
             , Agendas.undergroundMuscle
             , Agendas.chaosInTheCloverClub
             ]
          )
        )
    ResolveToken _ Tablet iid -> s <$ push (SpendResources iid 3)
    ResolveToken drawnToken Skull iid -> do
      let requiredResources = if isEasyStandard attrs then 2 else 3
      resourceCount <- field InvestigatorResources iid
      if resourceCount >= requiredResources
        then push $ chooseOne
          iid
          [ Label
            ("Spend "
            <> tshow requiredResources
            <> " resources to treat this token as a 0"
            )
            [ SpendResources iid requiredResources
            , CreateTokenValueEffect
              (if isEasyStandard attrs then 2 else 3)
              (TokenSource drawnToken)
              (TokenTarget drawnToken)
            ]
          , Label "Do not spend resources" []
          ]
        else pure ()
      pure s
    PassedSkillTest iid _ _ (TokenTarget token) _ _ ->
      s <$ case tokenFace token of
        Cultist | isEasyStandard attrs -> push $ TakeResources iid 3 False
        _ -> pure ()
    FailedSkillTest iid _ _ (TokenTarget token) _ _ ->
      s <$ case tokenFace token of
        Cultist | isHardExpert attrs -> push $ SpendResources iid 3
        Tablet | isEasyStandard attrs -> push $ SpendResources iid 3
        _ -> pure ()
    ScenarioResolution NoResolution ->
      s <$ push (ScenarioResolution $ Resolution 1)
    ScenarioResolution (Resolution 1) -> do
      investigatorIds <- getInvestigatorIds
      xp <- getXp
      pushAll
        $ [ story investigatorIds resolution1
         , Record OBannionGangHasABoneToPickWithTheInvestigators
         , Record DrFrancisMorganWasKidnapped
         ]
        <> [ AddToken ElderThing | Cheated `member` scenarioLog attrs ]
        <> [ GainXP iid (n + 1) | (iid, n) <- xp ]
        <> [EndOfGame Nothing]
      pure s
    ScenarioResolution (Resolution 2) -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      xp <- getXp
      pushAll
        $ [ story investigatorIds resolution2
          , Record OBannionGangHasABoneToPickWithTheInvestigators
          , Record TheInvestigatorsRescuedDrFrancisMorgan
          , chooseOne
            leadInvestigatorId
            [ Label
              "Add Dr. Francis Morgan to a deck"
              [ chooseOne
                  leadInvestigatorId
                  [ TargetLabel
                      (InvestigatorTarget iid)
                      [AddCampaignCardToDeck iid Assets.drFrancisMorgan]
                  | iid <- investigatorIds
                  ]
              ]
            , Label "Do not add Dr. Francis Morgan to any deck" []
            ]
          ]
        <> [ AddToken ElderThing | Cheated `member` scenarioLog attrs ]
        <> [ GainXP iid n | (iid, n) <- xp ]
        <> [EndOfGame Nothing]
      pure s
    ScenarioResolution (Resolution 3) -> do
      iids <- getInvestigatorIds
      xp <- getXp
      pushAll
        $ [ story iids resolution3
          , Record NaomiHasTheInvestigatorsBacks
          , Record DrFrancisMorganWasKidnapped
          ]
        <> [ AddToken ElderThing | Cheated `member` scenarioLog attrs ]
        <> [ GainXP iid n | (iid, n) <- xp ]
        <> [EndOfGame Nothing]
      pure s
    ScenarioResolution (Resolution 4) -> do
      iids <- getInvestigatorIds
      xp <- getXp
      pushAll
        $ [ story iids resolution4
          , Record OBannionGangHasABoneToPickWithTheInvestigators
          , Record DrFrancisMorganWasKidnapped
          , Record InvestigatorsWereUnconsciousForSeveralHours
          ]
        <> [ AddToken ElderThing | Cheated `member` scenarioLog attrs ]
        <> [ GainXP iid (n + 1) | (iid, n) <- xp ]
        <> [EndOfGame Nothing]
      pure s
    _ -> TheHouseAlwaysWins <$> runMessage msg attrs
