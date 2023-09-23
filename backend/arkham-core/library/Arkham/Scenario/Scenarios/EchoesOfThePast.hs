module Arkham.Scenario.Scenarios.EchoesOfThePast (
  EchoesOfThePast (..),
  echoesOfThePast,
) where

import Arkham.Prelude hiding (replicate)

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes hiding (matches)
import Arkham.Difficulty
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers
import Arkham.Id
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message
import Arkham.Modifier
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.EchoesOfThePast.Story
import Arkham.Token
import Arkham.Trait (Trait (SecondFloor, ThirdFloor))
import Arkham.Treachery.Cards qualified as Cards
import Data.List (replicate)

newtype EchoesOfThePast = EchoesOfThePast ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

echoesOfThePast :: Difficulty -> EchoesOfThePast
echoesOfThePast difficulty =
  scenario
    EchoesOfThePast
    "03120"
    "Echoes of the Past"
    difficulty
    [ "thirdFloor1  quietHalls2 thirdFloor2  . ."
    , "secondFloor1 quietHalls1 secondFloor2 . hiddenLibrary"
    , "groundFloor1 entryHall   groundFloor2 . ."
    ]

instance HasChaosTokenValue EchoesOfThePast where
  getChaosTokenValue iid chaosTokenFace (EchoesOfThePast attrs) = case chaosTokenFace of
    Skull -> do
      highestDoom <- fieldMax EnemyDoom AnyEnemy
      totalDoom <- selectSum EnemyDoom AnyEnemy
      pure $ toChaosTokenValue attrs Skull highestDoom totalDoom
    Cultist -> pure $ toChaosTokenValue attrs Cultist 2 4
    Tablet -> pure $ toChaosTokenValue attrs Tablet 2 4
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 2 4
    otherFace -> getChaosTokenValue iid otherFace attrs

gatherTheMidnightMasks :: CardGen m => m [EncounterCard]
gatherTheMidnightMasks =
  traverse
    genEncounterCard
    [ Cards.falseLead
    , Cards.falseLead
    , Cards.huntingShadow
    , Cards.huntingShadow
    , Cards.huntingShadow
    ]

placeAndLabelLocations :: Text -> [Card] -> GameT [(LocationId, [Message])]
placeAndLabelLocations prefix locations =
  for (withIndex1 locations) $ \(idx, location) -> do
    (locationId, placement) <- placeLocation location
    pure
      $ ( locationId
        , [placement, SetLocationLabel locationId (prefix <> tshow idx)]
        )

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
  , MinusFour
  , Skull
  , Skull
  , Skull
  , AutoFail
  , ElderSign
  ]

instance RunMessage EchoesOfThePast where
  runMessage msg s@(EchoesOfThePast attrs) = case msg of
    SetChaosTokensForScenario -> do
      -- TODO: move to helper since consistent
      standalone <- getIsStandalone
      randomToken <- sample (Cultist :| [Tablet, ElderThing])
      s
        <$ if standalone
          then push (SetChaosTokens $ standaloneChaosTokens <> [randomToken, randomToken])
          else pure ()
    Setup -> do
      investigatorIds <- allInvestigatorIds

      -- generate without seekerOfCarcosa as we add based on player count
      partialEncounterDeck <-
        buildEncounterDeckExcluding
          [ Enemies.possessedOathspeaker
          , Enemies.seekerOfCarcosa
          , Assets.mrPeabody
          ]
          [ EncounterSet.EchoesOfThePast
          , EncounterSet.CultOfTheYellowSign
          , EncounterSet.Delusions
          , EncounterSet.LockedDoors
          , EncounterSet.DarkCult
          ]
      midnightMasks <- gatherTheMidnightMasks
      (seekersToSpawn, seekersToShuffle) <-
        splitAt (length investigatorIds - 1)
          <$> traverse genEncounterCard (replicate 3 Enemies.seekerOfCarcosa)
      encounterDeck <-
        Deck
          <$> shuffleM
            (unDeck partialEncounterDeck <> midnightMasks <> seekersToShuffle)

      groundFloor <-
        genCards
          . drop 1
          =<< shuffleM
            [ Locations.historicalSocietyMeetingRoom
            , Locations.historicalSocietyRecordOffice_129
            , Locations.historicalSocietyHistoricalMuseum_130
            ]

      secondFloor <-
        genCards
          . drop 1
          =<< shuffleM
            [ Locations.historicalSocietyHistoricalMuseum_132
            , Locations.historicalSocietyHistoricalLibrary_133
            , Locations.historicalSocietyReadingRoom
            ]

      thirdFloor <-
        genCards
          . drop 1
          =<< shuffleM
            [ Locations.historicalSocietyHistoricalLibrary_136
            , Locations.historicalSocietyPeabodysOffice
            , Locations.historicalSocietyRecordOffice_138
            ]

      (entryHallId, placeEntryHall) <- placeLocationCard Locations.entryHall
      (quietHalls1Id, placeQuietHalls1) <- placeLocationCard Locations.quietHalls_131
      (quietHalls2Id, placeQuietHalls2) <- placeLocationCard Locations.quietHalls_135

      groundFloorPlacements <-
        shuffleM =<< placeAndLabelLocations "groundloor" groundFloor
      secondFloorPlacements <-
        shuffleM =<< placeAndLabelLocations "secondFloor" secondFloor
      thirdFloorPlacements <-
        shuffleM =<< placeAndLabelLocations "thirdFloor" thirdFloor

      spawnMessages <- case length seekersToSpawn of
        n | n == 3 -> for seekersToSpawn $ \seeker ->
          -- with 3 we can spawn at either 2nd or 3rd floor so we use
          -- location matching
          createEnemyAtLocationMatching_
            (EncounterCard seeker)
            ( EmptyLocation
                <> LocationMatchAny
                  [LocationWithTrait SecondFloor, LocationWithTrait ThirdFloor]
            )
        _ ->
          for (zip thirdFloorPlacements seekersToSpawn)
            $ \((locationId, _), card) ->
              createEnemyAt_ (EncounterCard card) locationId Nothing

      sebastienInterviewed <-
        elem (recorded $ toCardCode Assets.sebastienMoreau)
          <$> getRecordSet VIPsInterviewed

      fledTheDinnerParty <- getHasRecord YouFledTheDinnerParty

      pushAll
        $ [story investigatorIds intro]
        <> [ story investigatorIds sebastiensInformation
           | sebastienInterviewed
           ]
        <> [ SetEncounterDeck encounterDeck
           , SetAgendaDeck
           , SetActDeck
           , placeEntryHall
           ]
        <> [ PlaceTokens (toSource attrs) (LocationTarget entryHallId) Clue 1
           | sebastienInterviewed
           ]
        <> [ placeQuietHalls1
           , SetLocationLabel quietHalls1Id "quietHalls1"
           , placeQuietHalls2
           , SetLocationLabel quietHalls2Id "quietHalls2"
           ]
        <> concatMap snd groundFloorPlacements
        <> concatMap snd secondFloorPlacements
        <> concatMap snd thirdFloorPlacements
        <> spawnMessages
        <> [MoveAllTo (toSource attrs) entryHallId]
        <> ( if fledTheDinnerParty
              then
                [ CreateWindowModifierEffect
                  EffectRoundWindow
                  (EffectModifiers $ toModifiers attrs [AdditionalActions 1])
                  (toSource attrs)
                  (InvestigatorTarget iid)
                | iid <- investigatorIds
                ]
              else []
           )

      setAsideCards <-
        genCards
          [ Locations.hiddenLibrary
          , Enemies.possessedOathspeaker
          , Assets.mrPeabody
          , Assets.theTatteredCloak
          , Assets.claspOfBlackOnyx
          ]
      agendas <-
        genCards
          [ Agendas.theTruthIsHidden
          , Agendas.ransackingTheManor
          , Agendas.secretsBetterLeftHidden
          ]
      acts <-
        genCards
          [Acts.raceForAnswers, Acts.mistakesOfThePast, Acts.theOath]

      EchoesOfThePast
        <$> runMessage
          msg
          ( attrs
              & (setAsideCardsL .~ setAsideCards)
              & (actStackL . at 1 ?~ acts)
              & (agendaStackL . at 1 ?~ agendas)
          )
    ResolveChaosToken _ token iid | token `elem` [Cultist, Tablet, ElderThing] -> do
      case token of
        Cultist -> do
          matches <- selectListMap EnemyTarget (NearestEnemy AnyEnemy)
          push
            $ chooseOne
              iid
              [ targetLabel target [PlaceTokens (ChaosTokenEffectSource Cultist) target Doom 1] | target <- matches
              ]
        Tablet ->
          push $ toMessage $ randomDiscard iid (ChaosTokenEffectSource Tablet)
        ElderThing -> do
          triggers <- notNull <$> select (EnemyAt YourLocation)
          when triggers $ do
            push $ InvestigatorAssignDamage iid (ChaosTokenEffectSource token) DamageAny 0 1
        _ -> pure ()
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ | isEasyStandard attrs -> do
      case chaosTokenFace token of
        Cultist -> do
          matches <- selectListMap EnemyTarget (NearestEnemy AnyEnemy)
          push
            $ chooseOne
              iid
              [ targetLabel target [PlaceTokens (ChaosTokenEffectSource Cultist) target Doom 1] | target <- matches
              ]
        Tablet ->
          push $ toMessage $ randomDiscard iid (ChaosTokenEffectSource Tablet)
        ElderThing -> do
          triggers <- notNull <$> select (EnemyAt YourLocation)
          when triggers $ do
            push $ InvestigatorAssignDamage iid (ChaosTokenEffectSource $ chaosTokenFace token) DamageAny 0 1
        _ -> pure ()
      pure s
    ScenarioResolution NoResolution -> do
      investigatorIds <- allInvestigatorIds
      pushAll
        [story investigatorIds noResolution, ScenarioResolution (Resolution 4)]
      pure s
    ScenarioResolution (Resolution n) -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- allInvestigatorIds
      gainXp <- toGainXp attrs $ getXpWithBonus (if n == 4 then 1 else 0)
      sebastienSlain <-
        selectOne
          (VictoryDisplayCardMatch $ cardIs Enemies.sebastienMoreau)
      conviction <- getRecordCount Conviction
      doubt <- getRecordCount Doubt

      let
        updateSlain =
          [ recordSetInsert VIPsSlain [toCardCode sebastien]
          | sebastien <- maybeToList sebastienSlain
          ]
        removeTokens =
          [ RemoveAllChaosTokens Cultist
          , RemoveAllChaosTokens Tablet
          , RemoveAllChaosTokens ElderThing
          ]

      case n of
        1 ->
          pushAll
            $ [ story investigatorIds resolution1
              , Record YouTookTheOnyxClasp
              , RecordCount Conviction (conviction + 1)
              , chooseOne
                  leadInvestigatorId
                  [ TargetLabel
                    (InvestigatorTarget iid)
                    [AddCampaignCardToDeck iid Assets.claspOfBlackOnyx]
                  | iid <- investigatorIds
                  ]
              ]
            <> gainXp
            <> updateSlain
            <> removeTokens
            <> [AddChaosToken Cultist, AddChaosToken Cultist]
            <> [EndOfGame Nothing]
        2 ->
          pushAll
            $ [ story investigatorIds resolution2
              , Record YouLeftTheOnyxClaspBehind
              , RecordCount Doubt (doubt + 1)
              ]
            <> gainXp
            <> updateSlain
            <> removeTokens
            <> [AddChaosToken Tablet, AddChaosToken Tablet]
            <> [EndOfGame Nothing]
        3 ->
          pushAll
            $ [ story investigatorIds resolution3
              , Record YouDestroyedTheOathspeaker
              , chooseOne
                  leadInvestigatorId
                  [ TargetLabel
                    (InvestigatorTarget iid)
                    [AddCampaignCardToDeck iid Assets.theTatteredCloak]
                  | iid <- investigatorIds
                  ]
              ]
            <> gainXp
            <> updateSlain
            <> removeTokens
            <> [AddChaosToken Tablet, AddChaosToken Tablet]
            <> [EndOfGame Nothing]
        4 ->
          pushAll
            $ [ story investigatorIds resolution4
              , Record TheFollowersOfTheSignHaveFoundTheWayForward
              ]
            <> gainXp
            <> updateSlain
            <> removeTokens
            <> [AddChaosToken ElderThing, AddChaosToken ElderThing]
            <> [EndOfGame Nothing]
        _ -> error "Invalid resolution"
      pure s
    _ -> EchoesOfThePast <$> runMessage msg attrs
