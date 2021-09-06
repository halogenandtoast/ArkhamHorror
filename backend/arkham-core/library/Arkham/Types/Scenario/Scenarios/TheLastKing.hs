module Arkham.Types.Scenario.Scenarios.TheLastKing
  ( TheLastKing(..)
  , theLastKing
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Acts
import qualified Arkham.Agenda.Cards as Agendas
import qualified Arkham.Asset.Cards as Assets
import qualified Arkham.Enemy.Cards as Enemies
import qualified Arkham.Location.Cards as Locations
import Arkham.Scenarios.TheLastKing.Story
import Arkham.Types.Card
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Card.PlayerCard
import Arkham.Types.Classes
import Arkham.Types.Difficulty
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Scenario.Attrs
import Arkham.Types.Scenario.Helpers
import Arkham.Types.Scenario.Runner
import Arkham.Types.Target
import Arkham.Types.Token

newtype TheLastKing = TheLastKing ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theLastKing :: Difficulty -> TheLastKing
theLastKing difficulty =
  TheLastKing
    $ baseAttrs
        "03061"
        "The Last King"
        [Agendas.fashionablyLate, Agendas.theTerrifyingTruth]
        [Acts.discoveringTheTruth]
        difficulty
    & locationLayoutL
    ?~ [ "diningRoom .         gallery"
       , "ballroom   courtyard livingRoom"
       , ".          foyer     ."
       ]
instance HasRecord TheLastKing where
  hasRecord _ = pure False
  hasRecordSet _ = pure []
  hasRecordCount _ = pure 0

instance
  ( HasCount Shroud env LocationId
  , HasId LocationId env InvestigatorId
  , HasTokenValue env InvestigatorId
  )
  => HasTokenValue env TheLastKing where
  getTokenValue (TheLastKing attrs) iid = \case
    Skull -> pure $ TokenValue Skull NoModifier
    Cultist -> pure $ toTokenValue attrs Cultist 2 3
    Tablet -> pure $ TokenValue Tablet (NegativeModifier 4)
    ElderThing -> do
      lid <- getId @LocationId iid
      shroud <- unShroud <$> getCount lid
      pure $ TokenValue ElderThing (NegativeModifier shroud)
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
  , MinusFour
  , Skull
  , Skull
  , Skull
  , AutoFail
  , ElderSign
  ]

instance ScenarioRunner env => RunMessage env TheLastKing where
  runMessage msg s@(TheLastKing attrs) = case msg of
    SetTokensForScenario -> do
      standalone <- getIsStandalone
      randomToken <- sample (Cultist :| [Tablet, ElderThing])
      s <$ if standalone
        then push (SetTokens $ standaloneTokens <> [randomToken, randomToken])
        else pure ()
    StandaloneSetup -> do
      standalone <- getIsStandalone
      leadInvestigatorId <- getLeadInvestigatorId
      s <$ when
        standalone
        (push $ AddCampaignCardToDeck
          leadInvestigatorId
          Enemies.theManInThePallidMask
        )
    Setup -> do
      encounterDeck <- buildEncounterDeckExcluding
        [Enemies.dianneDevine]
        [EncounterSet.TheLastKing, EncounterSet.AncientEvils]
      foyerId <- getRandom
      courtyardId <- getRandom
      livingRoomId <- getRandom
      ballroomId <- getRandom
      diningRoomId <- getRandom
      galleryId <- getRandom

      totalClues <- getPlayerCountValue (StaticWithPerPlayer 1 1)

      bystanders <- shuffleM =<< traverse
        (fmap PlayerCard . genPlayerCard)
        [ Assets.constanceDumaine
        , Assets.jordanPerry
        , Assets.ishimaruHaruko
        , Assets.sebastienMoreau
        , Assets.ashleighClarke
        ]
      destinations <- shuffleM
        [courtyardId, livingRoomId, ballroomId, diningRoomId, galleryId]

      investigatorIds <- getInvestigatorIds

      pushAll
        ([ story investigatorIds intro
         , SetEncounterDeck encounterDeck
         , AddAgenda "03062"
         , AddAct "03064"
         , PlaceLocation foyerId Locations.foyer
         , PlaceLocation courtyardId Locations.courtyard
         , PlaceLocation livingRoomId Locations.livingRoom
         , PlaceLocation ballroomId Locations.ballroom
         , PlaceLocation diningRoomId Locations.diningRoom
         , PlaceLocation galleryId Locations.gallery
         , MoveAllTo (toSource attrs) foyerId
         ]
        <> zipWith CreateStoryAssetAt bystanders destinations
        <> map
             ((`PlaceClues` totalClues) . AssetTarget . AssetId . toCardId)
             bystanders
        )
      setAsideEncounterCards <- traverse
        (fmap EncounterCard . genEncounterCard)
        [Enemies.dianneDevine]
      TheLastKing
        <$> runMessage msg (attrs & setAsideCardsL .~ setAsideEncounterCards)
    _ -> TheLastKing <$> runMessage msg attrs
