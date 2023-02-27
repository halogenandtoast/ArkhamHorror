module Arkham.Scenario.Scenarios.EchoesOfThePast
  ( EchoesOfThePast(..)
  , echoesOfThePast
  ) where

import Arkham.Prelude hiding ( replicate )

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Classes
import Arkham.Difficulty
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Types ( Field (..) )
import Arkham.Enemy.Cards qualified as Enemies
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers
import Arkham.Id
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message
import Arkham.Modifier
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Helpers hiding ( matches )
import Arkham.Scenario.Runner
import Arkham.Scenarios.EchoesOfThePast.Story
import Arkham.Source
import Arkham.Target
import Arkham.Token
import Arkham.Trait ( Trait (SecondFloor, ThirdFloor) )
import Arkham.Treachery.Cards qualified as Cards
import Data.List ( replicate )

newtype EchoesOfThePast = EchoesOfThePast ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

echoesOfThePast :: Difficulty -> EchoesOfThePast
echoesOfThePast difficulty = scenario
  EchoesOfThePast
  "03120"
  "Echoes of the Past"
  difficulty
  [ "thirdFloor1  quietHalls2 thirdFloor2  . ."
  , "secondFloor1 quietHalls1 secondFloor2 . hiddenLibrary"
  , "groundFloor1 entryHall   groundFloor2 . ."
  ]

instance HasTokenValue EchoesOfThePast where
  getTokenValue iid tokenFace (EchoesOfThePast attrs) = case tokenFace of
    Skull -> do
      enemies <- selectList AnyEnemy
      doomCounts <- traverse (field EnemyDoom) enemies
      pure $ toTokenValue
        attrs
        Skull
        (maybe 0 maximum $ fromNullable doomCounts)
        (sum doomCounts)
    Cultist -> pure $ toTokenValue attrs Cultist 2 4
    Tablet -> pure $ toTokenValue attrs Tablet 2 4
    ElderThing -> pure $ toTokenValue attrs ElderThing 2 4
    otherFace -> getTokenValue iid otherFace attrs

gatherTheMidnightMasks :: CardGen m => m [EncounterCard]
gatherTheMidnightMasks = traverse
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
    pure $
      (locationId, [ placement
      , SetLocationLabel locationId (prefix <> tshow idx)
      ])

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
  , MinusFour
  , Skull
  , Skull
  , Skull
  , AutoFail
  , ElderSign
  ]

instance RunMessage EchoesOfThePast where
  runMessage msg s@(EchoesOfThePast attrs) = case msg of
    SetTokensForScenario -> do
      -- TODO: move to helper since consistent
      standalone <- getIsStandalone
      randomToken <- sample (Cultist :| [Tablet, ElderThing])
      s <$ if standalone
        then push (SetTokens $ standaloneTokens <> [randomToken, randomToken])
        else pure ()
    Setup -> do
      investigatorIds <- allInvestigatorIds

      -- generate without seekerOfCarcosa as we add based on player count
      partialEncounterDeck <- buildEncounterDeckExcluding
        [ toCardDef Enemies.possessedOathspeaker
        , toCardDef Enemies.seekerOfCarcosa
        , toCardDef Assets.mrPeabody
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
      encounterDeck <- Deck <$> shuffleM
        (unDeck partialEncounterDeck <> midnightMasks <> seekersToShuffle)

      groundFloor <- traverse genCard . drop 1 =<< shuffleM
        [ Locations.historicalSocietyMeetingRoom
        , Locations.historicalSocietyRecordOffice_129
        , Locations.historicalSocietyHistoricalMuseum_130
        ]

      secondFloor <- traverse genCard . drop 1 =<< shuffleM
        [ Locations.historicalSocietyHistoricalMuseum_132
        , Locations.historicalSocietyHistoricalLibrary_133
        , Locations.historicalSocietyReadingRoom
        ]

      thirdFloor <- traverse genCard . drop 1 =<< shuffleM
        [ Locations.historicalSocietyHistoricalLibrary_136
        , Locations.historicalSocietyPeabodysOffice
        , Locations.historicalSocietyRecordOffice_138
        ]

      (entryHallId, placeEntryHall) <- placeLocationCard Locations.entryHall
      (quietHalls1Id, placeQuietHalls1) <- placeLocationCard Locations.quietHalls_131
      (quietHalls2Id, placeQuietHalls2) <- placeLocationCard Locations.quietHalls_135

      groundFloorPlacements <- shuffleM =<< placeAndLabelLocations "groundloor" groundFloor
      secondFloorPlacements <- shuffleM =<< placeAndLabelLocations "secondFloor" secondFloor
      thirdFloorPlacements <- shuffleM =<< placeAndLabelLocations "thirdFloor" thirdFloor

      let
        spawnMessages = case length seekersToSpawn of
          n | n == 3 ->
            -- with 3 we can spawn at either 2nd or 3rd floor so we use
            -- location matching
            [ CreateEnemyAtLocationMatching
                (EncounterCard seeker)
                (EmptyLocation
                <> LocationMatchAny
                     [ LocationWithTrait SecondFloor
                     , LocationWithTrait ThirdFloor
                     ]
                )
            | seeker <- seekersToSpawn
            ]
          _ ->
            [ CreateEnemyAt (EncounterCard card) locationId Nothing
            | ((locationId, _), card) <- zip thirdFloorPlacements seekersToSpawn
            ]

      sebastienInterviewed <-
        elem (Recorded $ toCardCode Assets.sebastienMoreau)
          <$> getRecordSet VIPsInterviewed

      fledTheDinnerParty <- getHasRecord YouFledTheDinnerParty

      pushAll
        ([story investigatorIds intro]
        <> [ story investigatorIds sebastiensInformation
           | sebastienInterviewed
           ]
        <> [ SetEncounterDeck encounterDeck
           , SetAgendaDeck
           , SetActDeck
           , placeEntryHall
           ]
        <> [ PlaceClues (LocationTarget entryHallId) 1
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
        <> if fledTheDinnerParty
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

      setAsideCards <- traverse
        genCard
        [ toCardDef Locations.hiddenLibrary
        , toCardDef Enemies.possessedOathspeaker
        , toCardDef Assets.mrPeabody
        , toCardDef Assets.theTatteredCloak
        , toCardDef Assets.claspOfBlackOnyx
        ]

      EchoesOfThePast <$> runMessage
        msg
        (attrs
        & (setAsideCardsL .~ setAsideCards)
        & (actStackL
          . at 1
          ?~ [Acts.raceForAnswers, Acts.mistakesOfThePast, Acts.theOath]
          )
        & (agendaStackL
          . at 1
          ?~ [ Agendas.theTruthIsHidden
             , Agendas.ransackingTheManor
             , Agendas.secretsBetterLeftHidden
             ]
          )
        )
    ResolveToken _ token iid | token `elem` [Cultist, Tablet, ElderThing] ->
      s <$ case token of
        Cultist -> do
          matches <- selectListMap EnemyTarget (NearestEnemy AnyEnemy)
          push $ chooseOne
            iid
            [ TargetLabel target [PlaceDoom target 1] | target <- matches ]
        Tablet -> push $ RandomDiscard iid (TokenEffectSource Tablet) AnyCard
        ElderThing -> do
          triggers <- notNull <$> select (EnemyAt YourLocation)
          when
            triggers
            (push $ InvestigatorAssignDamage
              iid
              (TokenEffectSource token)
              DamageAny
              0
              1
            )
        _ -> pure ()
    FailedSkillTest iid _ _ (TokenTarget token) _ _ | isEasyStandard attrs -> do
      case tokenFace token of
        Cultist -> do
          matches <- selectListMap EnemyTarget (NearestEnemy AnyEnemy)
          push $ chooseOne
            iid
            [ TargetLabel target [PlaceDoom target 1] | target <- matches ]
        Tablet -> push $ RandomDiscard iid (TokenEffectSource Tablet) AnyCard
        ElderThing -> do
          triggers <- notNull <$> select (EnemyAt YourLocation)
          when
            triggers
            (push $ InvestigatorAssignDamage
              iid
              (TokenEffectSource $ tokenFace token)
              DamageAny
              0
              1
            )
        _ -> pure ()
      pure s
    ScenarioResolution NoResolution -> do
      investigatorIds <- allInvestigatorIds
      s
        <$ pushAll
             [ story investigatorIds noResolution
             , ScenarioResolution (Resolution 4)
             ]
    ScenarioResolution (Resolution n) -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- allInvestigatorIds
      gainXp <- map (uncurry GainXP)
        <$> getXpWithBonus (if n == 4 then 1 else 0)
      sebastienSlain <- selectOne
        (VictoryDisplayCardMatch $ cardIs Enemies.sebastienMoreau)
      conviction <- getRecordCount Conviction
      doubt <- getRecordCount Doubt

      let
        updateSlain =
          [ RecordSetInsert VIPsSlain [toCardCode sebastien]
          | sebastien <- maybeToList sebastienSlain
          ]
        removeTokens =
          [ RemoveAllTokens Cultist
          , RemoveAllTokens Tablet
          , RemoveAllTokens ElderThing
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
                    [AddCampaignCardToDeck iid $ toCardDef Assets.claspOfBlackOnyx]
                | iid <- investigatorIds
                ]
              ]
            <> gainXp
            <> updateSlain
            <> removeTokens
            <> [AddToken Cultist, AddToken Cultist]
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
            <> [AddToken Tablet, AddToken Tablet]
            <> [EndOfGame Nothing]
        3 ->
          pushAll
            $ [ story investigatorIds resolution3
              , Record YouDestroyedTheOathspeaker
              , chooseOne
                leadInvestigatorId
                [ TargetLabel
                    (InvestigatorTarget iid)
                    [AddCampaignCardToDeck iid $ toCardDef Assets.theTatteredCloak]
                | iid <- investigatorIds
                ]
              ]
            <> gainXp
            <> updateSlain
            <> removeTokens
            <> [AddToken Tablet, AddToken Tablet]
            <> [EndOfGame Nothing]
        4 ->
          pushAll
            $ [ story investigatorIds resolution4
              , Record TheFollowersOfTheSignHaveFoundTheWayForward
              ]
            <> gainXp
            <> updateSlain
            <> removeTokens
            <> [AddToken ElderThing, AddToken ElderThing]
            <> [EndOfGame Nothing]
        _ -> error "Invalid resolution"
      pure s
    _ -> EchoesOfThePast <$> runMessage msg attrs
