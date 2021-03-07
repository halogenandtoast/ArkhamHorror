module Arkham.Types.Scenario.Scenarios.LostInTimeAndSpace
  ( LostInTimeAndSpace(..)
  , lostInTimeAndSpace
  )
where

import Arkham.Prelude

import Arkham.Types.Card
import Arkham.Types.Card.EncounterCardMatcher
import Arkham.Types.Classes
import Arkham.Types.Difficulty
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.EnemyId
import Arkham.Types.EnemyMatcher
import Arkham.Types.Game.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.Scenario.Attrs
import Arkham.Types.Scenario.Helpers
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.Trait hiding (Cultist)

newtype LostInTimeAndSpace = LostInTimeAndSpace ScenarioAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostInTimeAndSpace :: Difficulty -> LostInTimeAndSpace
lostInTimeAndSpace difficulty = LostInTimeAndSpace $ baseAttrs
  "02311"
  "Lost in Time and Space"
  ["02312", "02313", "02314", "02315"]
  ["02316", "02317", "02318", "02319"]
  difficulty

instance HasRecord LostInTimeAndSpace where
  hasRecord _ _ = False
  hasRecordSet _ _ = []
  hasRecordCount _ _ = 0

instance
  ( HasSet LocationId env [Trait]
  , HasTokenValue env InvestigatorId
  , HasCount Shroud env LocationId
  , HasId LocationId env InvestigatorId
  )
  => HasTokenValue env LostInTimeAndSpace where
  getTokenValue (LostInTimeAndSpace attrs) iid = \case
    Skull -> do
      extradimensionalCount <- length <$> getSet @LocationId [Extradimensional]
      pure $ TokenValue
        Skull
        (NegativeModifier $ if isEasyStandard attrs
          then min extradimensionalCount 5
          else extradimensionalCount
        )
    Cultist -> pure $ TokenValue Cultist NoModifier
    Tablet -> pure $ toTokenValue attrs Tablet 3 5
    ElderThing -> do
      lid <- getId @LocationId iid
      shroud <- unShroud <$> getCount lid
      pure $ toTokenValue attrs ElderThing shroud (shroud * 2)
    otherFace -> getTokenValue attrs iid otherFace

standaloneTokens :: [Token]
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
  , MinusFive
  , Skull
  , Skull
  , Cultist
  , Tablet
  , ElderThing
  , AutoFail
  , ElderSign
  ]

lostInTimeAndSpaceIntro :: Message
lostInTimeAndSpaceIntro = FlavorText
  (Just "Scenario VII: Lost in Time and Space")
  [ "Passing through the gate is unlike anything you’ve\
    \ ever experienced. You feel your body twisting and\
    \ distorting, churning through realities as the gate\
    \ pulls you to its ultimate destination—a place\
    \ beyond the scope of human imagination."
  , "Suddenly, all is quiet and the chaos of the\
    \ journey is replaced with a sense of solitude and dread. You are in an\
    \ unfathomable place, vast beyond your ability to reason and utterly\
    \ alien besides. The landscape is surreal and strange, the architecture\
    \ impossible. You are so far from home that home has become a\
    \ threadbare dream you can barely recall. Even should you find a way out\
    \ of this awful place, you may never be the same again."
  ]

instance
  ( HasSet InvestigatorId env ()
  , ScenarioAttrsRunner env
  , HasId (Maybe EnemyId) env EnemyMatcher
  , HasId (Maybe LocationId) env LocationMatcher
  )
  => RunMessage env LostInTimeAndSpace where
  runMessage msg s@(LostInTimeAndSpace attrs) = case msg of
    SetTokensForScenario -> do
      standalone <- getIsStandalone
      s <$ if standalone
        then unshiftMessage (SetTokens standaloneTokens)
        else pure ()
    Setup -> do
      investigatorIds <- getInvestigatorIds
      encounterDeck <- buildEncounterDeckExcluding
        ["02323"]
        [ EncounterSet.LostInTimeAndSpace
        , EncounterSet.Sorcery
        , EncounterSet.TheBeyond
        , EncounterSet.HideousAbominations
        , EncounterSet.AgentsOfYogSothoth
        ]
      anotherDimensionId <- getRandom
      unshiftMessages
        [ story investigatorIds lostInTimeAndSpaceIntro
        , SetEncounterDeck encounterDeck
        , AddAgenda "02312"
        , AddAct "02316"
        , PlaceLocation "02320" anotherDimensionId
        , RevealLocation Nothing anotherDimensionId
        , MoveAllTo anotherDimensionId
        ]

      let
        locations' = mapFromList $ map
          (second pure . toFst (getLocationName . lookupLocationStub))
          [ "02320"
          , "02321"
          , "02322"
          , "02324"
          , "02325"
          , "02326"
          , "02327"
          , "02328"
          ]
      LostInTimeAndSpace <$> runMessage
        msg
        (attrs
        & (locationsL .~ locations')
        & (setAsideLocationsL .~ ["02321", "02322"])
        )
    After (PassedSkillTest iid _ _ (DrawnTokenTarget token) _ _) ->
      s <$ case (isHardExpert attrs, drawnTokenFace token) of
        (True, Cultist) -> unshiftMessage
          (DiscardEncounterUntilFirst
            (toSource attrs)
            (EncounterCardMatchByType (LocationType, Nothing))
          )
        (_, Tablet) -> do
          mYogSothothId <- getId (EnemyWithTitle "Yog-Sothoth")
          case mYogSothothId of
            Nothing -> pure ()
            Just eid -> unshiftMessage (EnemyAttack iid eid)
        _ -> pure ()
    After (FailedSkillTest iid _ _ (DrawnTokenTarget token) _ _) ->
      s <$ case drawnTokenFace token of
        Cultist -> unshiftMessage
          (DiscardEncounterUntilFirst
            (ProxySource (toSource attrs) (InvestigatorSource iid))
            (EncounterCardMatchByType (LocationType, Nothing))
          )
        Tablet -> do
          mYogSothothId <- getId (EnemyWithTitle "Yog-Sothoth")
          case mYogSothothId of
            Nothing -> pure ()
            Just eid -> unshiftMessage (EnemyAttack iid eid)
        _ -> pure ()
    RequestedEncounterCard (ProxySource source (InvestigatorSource iid)) mcard
      | isSource attrs source -> s <$ case mcard of
        Nothing -> pure ()
        Just card -> do
          locationId <- getRandom
          unshiftMessages
            [PlaceLocation (ecCardCode card) locationId, MoveTo iid locationId]
    ScenarioResolution NoResolution -> pure s
    _ -> LostInTimeAndSpace <$> runMessage msg attrs
