module Arkham.Types.Scenario.Scenarios.EchoesOfThePast
  ( EchoesOfThePast(..)
  , echoesOfThePast
  ) where

import Arkham.Prelude hiding (replicate)

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Locations
import Arkham.Scenarios.EchoesOfThePast.Story
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Types.CampaignLogKey
import Arkham.Types.Card
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Classes
import Arkham.Types.Difficulty
import Arkham.Types.Effect.Window
import Arkham.Types.EffectMetadata
import Arkham.Types.EncounterSet qualified as EncounterSet
import Arkham.Types.Helpers
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.Scenario.Attrs
import Arkham.Types.Scenario.Helpers
import Arkham.Types.Scenario.Runner
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.Trait (Trait(SecondFloor, ThirdFloor))
import Data.List (replicate)

newtype EchoesOfThePast = EchoesOfThePast ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

echoesOfThePast :: Difficulty -> EchoesOfThePast
echoesOfThePast difficulty =
  EchoesOfThePast
    $ baseAttrs
        "03120"
        "Echoes of the Past"
        [ Agendas.theTruthIsHidden
        , Agendas.ransackingTheManor
        , Agendas.secretsBetterLeftHidden
        ]
        [Acts.raceForAnswers, Acts.mistakesOfThePast, Acts.theOath]
        difficulty
    & locationLayoutL
    ?~ [ "thirdFloor1  quietHalls2 thirdFloor2  . ."
       , "secondFloor1 quietHalls1 secondFloor2 . hiddenLibrary"
       , "groundFloor1 entryHall   groundFloor2 . ."
       ]

instance HasRecord EchoesOfThePast where
  hasRecord _ = pure False
  hasRecordSet _ = pure []
  hasRecordCount _ = pure 0

instance
  ( Query EnemyMatcher env
  , HasCount DoomCount env EnemyId
  , HasTokenValue env InvestigatorId
  )
  => HasTokenValue env EchoesOfThePast where
  getTokenValue (EchoesOfThePast attrs) iid = \case
    Skull -> do
      enemies <- selectList AnyEnemy
      doomCounts <- traverse (fmap unDoomCount . getCount) enemies
      pure $ toTokenValue
        attrs
        Skull
        (maybe 0 maximum $ fromNullable doomCounts)
        (sum doomCounts)
    Cultist -> pure $ toTokenValue attrs Cultist 2 4
    Tablet -> pure $ toTokenValue attrs Tablet 2 4
    ElderThing -> pure $ toTokenValue attrs ElderThing 2 4
    otherFace -> getTokenValue attrs iid otherFace

gatherTheMidnightMasks :: MonadRandom m => m [EncounterCard]
gatherTheMidnightMasks = traverse
  genEncounterCard
  [ Cards.falseLead
  , Cards.falseLead
  , Cards.huntingShadow
  , Cards.huntingShadow
  , Cards.huntingShadow
  ]

placeAndLabelLocations :: Text -> [Card] -> [Message]
placeAndLabelLocations prefix locations = concat
  [ [ PlaceLocation location
    , SetLocationLabel (toLocationId location) (prefix <> tshow @Int n)
    ]
  | (location, n) <- zip locations [1 ..]
  ]

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

instance ScenarioRunner env => RunMessage env EchoesOfThePast where
  runMessage msg s@(EchoesOfThePast attrs) = case msg of
    SetTokensForScenario -> do
      -- TODO: move to helper since consistent
      standalone <- getIsStandalone
      randomToken <- sample (Cultist :| [Tablet, ElderThing])
      s <$ if standalone
        then push (SetTokens $ standaloneTokens <> [randomToken, randomToken])
        else pure ()
    Setup -> do
      investigatorIds <- getInvestigatorIds

      -- generate without seekerOfCarcosa as we add based on player count
      partialEncounterDeck <- buildEncounterDeckExcluding
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

      entryHall <- genCard Locations.entryHall
      quietHalls1 <- genCard Locations.quietHalls_131
      quietHalls2 <- genCard Locations.quietHalls_135

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
            [ CreateEnemyAt (EncounterCard card) (toLocationId location) Nothing
            | (location, card) <- zip thirdFloor seekersToSpawn
            ]

      sebastienInterviewed <- elem (Recorded "03079")
        <$> hasRecordSet VIPsInterviewed

      fledTheDinnerParty <- hasRecord YouFledTheDinnerParty

      pushAll
        ([story investigatorIds intro]
        <> [ story investigatorIds sebastiensInformation
           | sebastienInterviewed
           ]
        <> [ SetEncounterDeck encounterDeck
           , AddAgenda "03121"
           , AddAct "03124"
           , PlaceLocation entryHall
           ]
        <> [ PlaceClues (LocationTarget $ toLocationId entryHall) 1
           | sebastienInterviewed
           ]
        <> [ PlaceLocation quietHalls1
           , SetLocationLabel (toLocationId quietHalls1) "quietHalls1"
           , PlaceLocation quietHalls2
           , SetLocationLabel (toLocationId quietHalls2) "quietHalls2"
           ]
        <> placeAndLabelLocations "groundFloor" groundFloor
        <> placeAndLabelLocations "secondFloor" secondFloor
        <> placeAndLabelLocations "thirdFloor" thirdFloor
        <> spawnMessages
        <> [MoveAllTo (toSource attrs) (toLocationId entryHall)]
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
        [ Locations.hiddenLibrary
        , Enemies.possessedOathspeaker
        , Assets.mrPeabody
        , Assets.theTatteredCloak
        , Assets.claspOfBlackOnyx
        ]

      EchoesOfThePast
        <$> runMessage msg (attrs & (setAsideCardsL .~ setAsideCards))
    ResolveToken _ token iid -> s <$ case token of
      Cultist -> do
        matches <- selectListMap EnemyTarget (NearestEnemy AnyEnemy)
        push $ chooseOne
          iid
          [ TargetLabel target [PlaceDoom target 1] | target <- matches ]
      Tablet -> push $ RandomDiscard iid
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
        Tablet -> push $ RandomDiscard iid
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
      investigatorIds <- getInvestigatorIds
      s
        <$ pushAll
             [ story investigatorIds noResolution
             , ScenarioResolution (Resolution 4)
             ]
    ScenarioResolution (Resolution n) -> do
      investigatorIds <- getInvestigatorIds
      case n of
        1 -> s <$ pushAll [story investigatorIds resolution1]
        2 -> s <$ pushAll [story investigatorIds resolution2]
        3 -> s <$ pushAll [story investigatorIds resolution3]
        4 -> s <$ pushAll [story investigatorIds resolution4]
        _ -> error "Invalid resolution"
    _ -> EchoesOfThePast <$> runMessage msg attrs
