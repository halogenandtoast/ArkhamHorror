{-# LANGUAGE TemplateHaskell #-}

module Arkham.Scenario.Types (
  module Arkham.Scenario.Types,
  module X,
  Field (..),
) where

import Arkham.Prelude

import Arkham.CampaignLog
import Arkham.Card
import Arkham.ChaosBag.Base
import Arkham.Classes.Entity
import Arkham.Classes.HasModifiersFor
import Arkham.Classes.HasTokenValue
import Arkham.Classes.RunMessage.Internal
import Arkham.Difficulty
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers
import Arkham.Id
import Arkham.Json
import Arkham.Name
import Arkham.Projection
import Arkham.Scenario.Deck as X
import Arkham.ScenarioLogKey
import Arkham.Source
import Arkham.Target
import Data.Aeson.TH
import Data.Typeable

class
  ( Typeable a
  , ToJSON a
  , FromJSON a
  , Eq a
  , Show a
  , HasModifiersFor a
  , RunMessage a
  , HasTokenValue a
  , Entity a
  , EntityId a ~ ScenarioId
  , EntityAttrs a ~ ScenarioAttrs
  ) =>
  IsScenario a

newtype GridTemplateRow = GridTemplateRow {unGridTemplateRow :: Text}
  deriving newtype (Show, IsString, ToJSON, FromJSON, Eq)

data instance Field Scenario :: Type -> Type where
  ScenarioCardsUnderActDeck :: Field Scenario [Card]
  ScenarioCardsUnderAgendaDeck :: Field Scenario [Card]
  ScenarioCardsUnderScenarioReference :: Field Scenario [Card]
  ScenarioDiscard :: Field Scenario [EncounterCard]
  ScenarioEncounterDeck :: Field Scenario (Deck EncounterCard)
  ScenarioDifficulty :: Field Scenario Difficulty
  ScenarioDecks :: Field Scenario (Map ScenarioDeckKey [Card])
  ScenarioVictoryDisplay :: Field Scenario [Card]
  ScenarioRemembered :: Field Scenario (Set ScenarioLogKey)
  ScenarioCounts :: Field Scenario (Map ScenarioCountKey Int)
  ScenarioStandaloneCampaignLog :: Field Scenario CampaignLog
  ScenarioResignedCardCodes :: Field Scenario [CardCode]
  ScenarioChaosBag :: Field Scenario ChaosBag
  ScenarioSetAsideCards :: Field Scenario [Card]
  ScenarioName :: Field Scenario Name
  ScenarioMeta :: Field Scenario Value
  ScenarioStoryCards :: Field Scenario (Map InvestigatorId [PlayerCard])
  ScenarioPlayerDecks :: Field Scenario (Map InvestigatorId (Deck PlayerCard))

deriving stock instance Show (Field Scenario typ)

data ScenarioAttrs = ScenarioAttrs
  { scenarioName :: Name
  , scenarioId :: ScenarioId
  , scenarioReference :: CardCode
  , scenarioDifficulty :: Difficulty
  , scenarioCardsUnderScenarioReference :: [Card]
  , scenarioCardsUnderAgendaDeck :: [Card]
  , scenarioCardsUnderActDeck :: [Card]
  , scenarioCardsNextToActDeck :: [Card]
  , scenarioActStack :: IntMap [Card]
  , scenarioAgendaStack :: IntMap [Card]
  , scenarioCompletedAgendaStack :: IntMap [Card]
  , scenarioCompletedActStack :: IntMap [Card]
  , scenarioLocationLayout :: [GridTemplateRow]
  , scenarioDecks :: Map ScenarioDeckKey [Card]
  , scenarioLog :: Set ScenarioLogKey
  , scenarioCounts :: Map ScenarioCountKey Int
  , scenarioStandaloneCampaignLog :: CampaignLog
  , scenarioSetAsideCards :: [Card]
  , scenarioInResolution :: Bool
  , scenarioNoRemainingInvestigatorsHandler :: Target
  , scenarioVictoryDisplay :: [Card]
  , scenarioChaosBag :: ChaosBag
  , scenarioEncounterDeck :: Deck EncounterCard
  , scenarioDiscard :: [EncounterCard]
  , scenarioEncounterDecks :: Map ScenarioEncounterDeckKey (Deck EncounterCard, [EncounterCard])
  , scenarioActiveEncounterDeck :: ScenarioEncounterDeckKey
  , scenarioResignedCardCodes :: [CardCode]
  , scenarioDecksLayout :: [GridTemplateRow]
  , scenarioMeta :: Value
  , -- for standalone
    scenarioStoryCards :: Map InvestigatorId [PlayerCard]
  , scenarioPlayerDecks :: Map InvestigatorId (Deck PlayerCard)
  }
  deriving stock (Show, Eq)

newtype ScenarioBehaviors = ScenarioBehaviors
  { scenarioHandleDiscard
      :: forall m
       . (HasGame m, MonadIO m)
      => EncounterCard
      -> ScenarioAttrs
      -> m ScenarioAttrs
  }

defaultScenarioBehaviors :: ScenarioBehaviors
defaultScenarioBehaviors =
  ScenarioBehaviors
    { scenarioHandleDiscard = \c attrs -> do
        pure $ attrs {scenarioDiscard = c : scenarioDiscard attrs}
    }

scenarioWith
  :: (ScenarioAttrs -> a)
  -> CardCode
  -> Name
  -> Difficulty
  -> [GridTemplateRow]
  -> (ScenarioAttrs -> ScenarioAttrs)
  -> a
scenarioWith f cardCode name difficulty layout g =
  scenario (f . g) cardCode name difficulty layout

scenario
  :: (ScenarioAttrs -> a)
  -> CardCode
  -> Name
  -> Difficulty
  -> [GridTemplateRow]
  -> a
scenario f cardCode name difficulty layout =
  f $
    ScenarioAttrs
      { scenarioId = ScenarioId cardCode
      , scenarioReference = cardCode
      , scenarioName = name
      , scenarioDifficulty = difficulty
      , scenarioCompletedAgendaStack = mempty
      , scenarioCompletedActStack = mempty
      , scenarioAgendaStack = mempty
      , scenarioActStack = mempty
      , scenarioCardsUnderAgendaDeck = mempty
      , scenarioCardsUnderActDeck = mempty
      , scenarioCardsNextToActDeck = mempty
      , scenarioLocationLayout = layout
      , scenarioDecks = mempty
      , scenarioLog = mempty
      , scenarioCounts = mempty
      , scenarioSetAsideCards = mempty
      , scenarioStandaloneCampaignLog = mkCampaignLog
      , scenarioCardsUnderScenarioReference = mempty
      , scenarioInResolution = False
      , scenarioNoRemainingInvestigatorsHandler = ScenarioTarget
      , scenarioVictoryDisplay = mempty
      , scenarioChaosBag = emptyChaosBag
      , scenarioEncounterDeck = mempty
      , scenarioEncounterDecks = mempty
      , scenarioActiveEncounterDeck = RegularEncounterDeck
      , scenarioDiscard = mempty
      , scenarioResignedCardCodes = mempty
      , scenarioDecksLayout = ["agenda1 act1"]
      , scenarioMeta = Null
      , scenarioStoryCards = mempty
      , scenarioPlayerDecks = mempty
      }

instance Entity ScenarioAttrs where
  type EntityId ScenarioAttrs = ScenarioId
  type EntityAttrs ScenarioAttrs = ScenarioAttrs
  toId = scenarioId
  toAttrs = id
  overAttrs f = f

instance Named ScenarioAttrs where
  toName = scenarioName

instance Targetable ScenarioAttrs where
  toTarget _ = ScenarioTarget
  isTarget _ ScenarioTarget = True
  isTarget _ _ = False

instance Sourceable ScenarioAttrs where
  toSource _ = ScenarioSource
  isSource _ ScenarioSource = True
  isSource _ _ = False

data Scenario where
  Scenario :: (IsScenario a) => a -> Scenario

instance Targetable Scenario where
  toTarget _ = ScenarioTarget

instance Eq Scenario where
  Scenario (a :: a) == Scenario (b :: b) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance Show Scenario where
  show (Scenario a) = show a

instance ToJSON Scenario where
  toJSON (Scenario a) = toJSON a

instance HasModifiersFor Scenario where
  getModifiersFor target (Scenario a) = getModifiersFor target a

instance Entity Scenario where
  type EntityId Scenario = ScenarioId
  type EntityAttrs Scenario = ScenarioAttrs
  toId = toId . toAttrs
  toAttrs (Scenario a) = toAttrs a
  overAttrs f (Scenario a) = Scenario $ overAttrs f a

difficultyOfScenario :: Scenario -> Difficulty
difficultyOfScenario = scenarioDifficulty . toAttrs

scenarioActs :: Scenario -> [Card]
scenarioActs s = case mapToList $ scenarioActStack (toAttrs s) of
  [(_, actIds)] -> actIds
  _ -> error "Not able to handle multiple act stacks yet"

makeLensesWith suffixedFields ''ScenarioAttrs
$(deriveJSON (aesonOptions $ Just "scenario") ''ScenarioAttrs)
