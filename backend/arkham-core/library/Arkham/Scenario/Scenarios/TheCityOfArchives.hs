module Arkham.Scenario.Scenarios.TheCityOfArchives
  ( TheCityOfArchives(..)
  , theCityOfArchives
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.CampaignStep
import Arkham.Card
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Deck
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TheCityOfArchives.Story
import Arkham.Source
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Trait hiding ( Trait (Cultist) )
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window
import Control.Lens ( over )

newtype TheCityOfArchives = TheCityOfArchives ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCityOfArchives :: Difficulty -> TheCityOfArchives
theCityOfArchives difficulty = scenario
  TheCityOfArchives
  "04237"
  "The City of Archives"
  difficulty
  [ ".                yithianOrrery                   laboratoryOfTheGreatRace         deconstructionRoom              ."
  , ".                .                               hallsOfPnakotusNorthernCorridors .                               interviewRoom1"
  , "towersOfPnakotus hallsOfPnakotusWesternCorridors .                                hallsOfPnakotusEasternCorridors interviewRoom2"
  , ".                greatLibrary                    .                                .                               interviewRoom3"
  ]

instance HasTokenValue TheCityOfArchives where
  getTokenValue iid tokenFace (TheCityOfArchives attrs) = case tokenFace of
    Skull -> do
      cardsInHand <- fieldMap InvestigatorHand length iid
      pure $ if cardsInHand >= 5
        then TokenValue Skull $ if isEasyStandard attrs
          then NegativeModifier 3
          else AutoFailModifier
        else toTokenValue attrs Skull 1 2
    Cultist -> pure $ TokenValue Cultist $ NegativeModifier 2
    Tablet -> pure $ TokenValue Tablet $ NegativeModifier 3
    ElderThing -> pure $ TokenValue ElderThing $ NegativeModifier 2
    otherFace -> getTokenValue iid otherFace attrs

standaloneTokens :: [TokenFace]
standaloneTokens =
  [ PlusOne
  , Zero
  , Zero
  , Zero
  , MinusOne
  , MinusTwo
  , MinusTwo
  , MinusThree
  , MinusFive
  , Skull
  , Skull
  , Cultist
  , ElderThing
  , AutoFail
  , ElderSign
  ]

instance RunMessage TheCityOfArchives where
  runMessage msg s@(TheCityOfArchives attrs) = case msg of
    SetTokensForScenario -> do
      whenM getIsStandalone $ push $ SetTokens standaloneTokens
      pure s
    CheckWindow _ [Window Timing.When (Window.DrawingStartingHand iid)] -> do
      uniqueItemAssetCards <-
        selectList $ InDeckOf (InvestigatorWithId iid) <> BasicCardMatch
          (CardWithTrait Item <> CardIsUnique)
      uniqueItemAssets <- selectList $ AssetWithTrait Item <> UniqueAsset

      mAlejandro <-
        selectOne $ InDeckOf (InvestigatorWithId iid) <> BasicCardMatch
          (cardIs Assets.alejandroVela)

      let setAsideUpdate = maybe id (over setAsideCardsL . (:)) mAlejandro

      pushAll
        $ map (RemovePlayerCardFromGame True) uniqueItemAssetCards
        <> [ RemovePlayerCardFromGame True alejandro
           | alejandro <- maybeToList mAlejandro
           ]
        <> map (RemoveFromGame . AssetTarget) uniqueItemAssets
      pure . TheCityOfArchives $ attrs & setAsideUpdate
    Setup -> do
      iids <- allInvestigatorIds
      leadInvestigator <- getLeadInvestigatorId
      pushAll
        $ map BecomeYithian iids
        <> [ story iids intro1
           , chooseOne
             leadInvestigator
             [ Label
               "Cooperate and tell the creatures everything you know."
               [ story iids intro2
               , Record TheInvestigatorsCooperatedWithTheYithians
               ]
             , Label
               "Refuse and resist captivity."
               [story iids intro3, Record TheInvestigatorsResistedCaptivity]
             ]
           , SetupStep (toTarget attrs) 1
           ]
      TheCityOfArchives <$> runMessage msg attrs
    SetupStep (isTarget attrs -> True) 1 -> do
      cooperatedWithTheYithians <- getHasRecord
        TheInvestigatorsCooperatedWithTheYithians
      interviewRoom <- genCard $ if cooperatedWithTheYithians
        then Locations.interviewRoomArrivalChamber
        else Locations.interviewRoomRestrainingChamber
      otherRooms <- traverse genCard =<< shuffleM
        [ Locations.interviewRoomIchorFilledChamber
        , if cooperatedWithTheYithians
          then Locations.interviewRoomRestrainingChamber
          else Locations.interviewRoomArrivalChamber
        ]

      encounterDeck' <- buildEncounterDeck
        [ EncounterSet.TheCityOfArchives
        , EncounterSet.AgentsOfYogSothoth
        , EncounterSet.LockedDoors
        , EncounterSet.ChillingCold
        , EncounterSet.StrikingFear
        ]

      yithianObserver <- genCard Enemies.yithianObserver
      placeRemainingLocations <- traverse
        placeLocationCard_
        [ Locations.hallsOfPnakotusNorthernCorridors
        , Locations.hallsOfPnakotusEasternCorridors
        , Locations.hallsOfPnakotusWesternCorridors
        ]

      setAsideCards <- traverse
        genCard
        [ Locations.greatLibrary
        , Locations.yithianOrrery
        , Locations.laboratoryOfTheGreatRace
        , Locations.deconstructionRoom
        , Locations.towersOfPnakotus
        , Assets.theCustodian
        ]

      let
        encounterDeck =
          removeEachFromDeck encounterDeck' [Enemies.yithianObserver]
        victoryDisplayUpdate = if cooperatedWithTheYithians
          then id
          else victoryDisplayL %~ (yithianObserver :)

      (interviewRoomId, placeInterviewRoom) <- placeLocation interviewRoom
      placeOtherRooms <- for (zip [2 ..] otherRooms) $ \(idx, location) -> do
        (locationId, placement) <- placeLocation location
        pure
          [ placement
          , SetLocationLabel locationId ("interviewRoom" <> tshow @Int idx)
          ]

      pushAll
        $ [ SetEncounterDeck encounterDeck
          , SetAgendaDeck
          , SetActDeck
          , placeInterviewRoom
          , SetLocationLabel interviewRoomId "interviewRoom1"
          , MoveAllTo (toSource attrs) interviewRoomId
          ]
        <> concat placeOtherRooms
        <> [ CreateEnemyAt yithianObserver interviewRoomId Nothing
           | cooperatedWithTheYithians
           ]
        <> placeRemainingLocations
      pure
        . TheCityOfArchives
        $ attrs
        & victoryDisplayUpdate
        & (setAsideCardsL %~ (<> setAsideCards))
        & (agendaStackL
          . at 1
          ?~ [ Agendas.cityOfTheGreatRace
             , Agendas.lostMemories
             , Agendas.humanityFading
             ]
          )
        & (actStackL
          . at 1
          ?~ [ Acts.exploringPnakotus
             , Acts.restrictedAccess
             , Acts.repossession
             ]
          )
    ResolveToken _ tokenFace iid
      | isHardExpert attrs && tokenFace `elem` [Cultist, ElderThing] -> do
        push $ InvestigatorPlaceCluesOnLocation iid 1
        pure s
    FailedSkillTest iid _ _ (TokenTarget token) _ n -> do
      case tokenFace token of
        face | face `elem` [Cultist, ElderThing] ->
          push $ InvestigatorPlaceCluesOnLocation iid 1
        Tablet -> do
          let discardCount = if isEasyStandard attrs then 1 else n
          pushAll $ replicate discardCount $ RandomDiscard
            iid
            (TokenEffectSource Tablet)
            AnyCard
        _ -> pure ()
      pure s
    ScenarioResolution r -> do
      iids <- allInvestigatorIds
      case r of
        NoResolution ->
          pushAll
            $ [ story iids noResolution
              , Record TheInvestigatorsHadTheirMemoriesExpunged
              ]
            <> map DrivenInsane iids
            <> [GameOver]
        Resolution 1 -> do
          rememberedTasks <- countM
            remembered
            [ FoundTheProcess
            , DissectedAnOrgan
            , InterviewedASubject
            , RealizedWhatYearItIs
            , ActivatedTheDevice
            ]
          resignedWithTheCustodian <- orM
            [ resignedWith Assets.theCustodian
            , selectAny (AssetControlledBy Anyone <> assetIs Assets.theCustodian)
            ]

          let
            totalTasks =
              rememberedTasks + if resignedWithTheCustodian then 1 else 0
            (logEntry, bonusXp) = case totalTasks of
              n | n == 6 -> (TheProcessWasPerfected, 4)
              n | n == 5 -> (TheProcessWasSuccessful, 2)
              n | n == 4 -> (TheProcessBackfired, 1)
              n | n == 3 -> (TheProcessBackfiredSpectacularly, 0)
              _ -> error "Invalid number of tasks"

          gainXp <- map (uncurry GainXP) <$> getXpWithBonus bonusXp

          let
            interludeResult = if resignedWithTheCustodian
              then Just TheCustodianWasUnderControl
              else Nothing

          pushAll
            $ story iids resolution1
            : Record logEntry
            : gainXp
            <> [EndOfGame (Just $ InterludeStep 4 interludeResult)]
        _ -> error "Invalid resolution"
      pure s
    _ -> TheCityOfArchives <$> runMessage msg attrs
