{-# LANGUAGE TemplateHaskell #-}
module Arkham.Scenario.Attrs
  ( module Arkham.Scenario.Attrs
  , module X
  , Field(..)
  ) where

import Arkham.Prelude

import Arkham.CampaignLog
import Arkham.Card
import Arkham.ChaosBag.Base
import Arkham.Classes.HasModifiersFor
import Arkham.Classes.HasTokenValue
import Arkham.Classes.RunMessage.Internal
import Arkham.Classes.Entity
import Arkham.Difficulty
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

class (Typeable a, ToJSON a, FromJSON a, Eq a, Show a, HasModifiersFor a, RunMessage a, HasTokenValue a, Entity a, EntityId a ~ ScenarioId, EntityAttrs a ~ ScenarioAttrs) => IsScenario a

newtype GridTemplateRow = GridTemplateRow { unGridTemplateRow :: Text }
  deriving newtype (Show, IsString, ToJSON, FromJSON, Eq)

data instance Field ScenarioAttrs :: Type -> Type where
  ScenarioCardsUnderActDeck :: Field ScenarioAttrs [Card]
  ScenarioCardsUnderAgendaDeck :: Field ScenarioAttrs [Card]
  ScenarioCardsUnderScenarioReference :: Field ScenarioAttrs [Card]
  ScenarioDiscard :: Field ScenarioAttrs [EncounterCard]
  ScenarioEncounterDeck :: Field ScenarioAttrs (Deck EncounterCard)
  ScenarioDifficulty :: Field ScenarioAttrs Difficulty
  ScenarioDecks :: Field ScenarioAttrs (HashMap ScenarioDeckKey [Card])
  ScenarioVictoryDisplay :: Field ScenarioAttrs [Card]
  ScenarioRemembered :: Field ScenarioAttrs (HashSet ScenarioLogKey)
  ScenarioStandaloneCampaignLog :: Field ScenarioAttrs CampaignLog
  ScenarioResignedCardCodes :: Field ScenarioAttrs [CardCode]
  ScenarioChaosBag :: Field ScenarioAttrs ChaosBag
  ScenarioSetAsideCards :: Field ScenarioAttrs [Card]
  ScenarioName :: Field ScenarioAttrs Name
  ScenarioStoryCards :: Field ScenarioAttrs (HashMap InvestigatorId [PlayerCard])

data ScenarioAttrs = ScenarioAttrs
  { scenarioName :: Name
  , scenarioId :: ScenarioId
  , scenarioDifficulty :: Difficulty
  , scenarioCardsUnderScenarioReference :: [Card]
  , scenarioCardsUnderAgendaDeck :: [Card]
  , scenarioCardsUnderActDeck :: [Card]
  , scenarioCardsNextToActDeck :: [Card]
  , scenarioActStack :: IntMap [CardDef]
  , scenarioAgendaStack :: IntMap [CardDef]
  , scenarioCompletedAgendaStack :: IntMap [CardDef]
  , scenarioLocationLayout :: Maybe [GridTemplateRow]
  , scenarioDecks :: HashMap ScenarioDeckKey [Card]
  , scenarioLog :: HashSet ScenarioLogKey
  , scenarioStandaloneCampaignLog :: CampaignLog
  , scenarioSetAsideCards :: [Card]
  , scenarioInResolution :: Bool
  , scenarioNoRemainingInvestigatorsHandler :: Target
  , scenarioVictoryDisplay :: [Card]
  , scenarioChaosBag :: ChaosBag
  , scenarioEncounterDeck :: Deck EncounterCard
  , scenarioDiscard :: [EncounterCard]
  , scenarioResignedCardCodes :: [CardCode]
  -- for standalone
  , scenarioStoryCards :: HashMap InvestigatorId [PlayerCard]
  }
  deriving stock (Show, Eq)

$(deriveJSON (aesonOptions $ Just "Scenario") ''ScenarioAttrs)
makeLensesWith suffixedFields ''ScenarioAttrs

baseAttrs :: CardCode -> Name -> Difficulty -> ScenarioAttrs
baseAttrs cardCode name difficulty = ScenarioAttrs
  { scenarioId = ScenarioId cardCode
  , scenarioName = name
  , scenarioDifficulty = difficulty
  , scenarioCompletedAgendaStack = mempty
  , scenarioAgendaStack = mempty
  , scenarioActStack = mempty
  , scenarioCardsUnderAgendaDeck = mempty
  , scenarioCardsUnderActDeck = mempty
  , scenarioCardsNextToActDeck = mempty
  , scenarioLocationLayout = Nothing
  , scenarioDecks = mempty
  , scenarioLog = mempty
  , scenarioSetAsideCards = mempty
  , scenarioStandaloneCampaignLog = mkCampaignLog
  , scenarioCardsUnderScenarioReference = mempty
  , scenarioInResolution = False
  , scenarioNoRemainingInvestigatorsHandler = ScenarioTarget
    (ScenarioId cardCode)
  , scenarioVictoryDisplay = mempty
  , scenarioChaosBag = emptyChaosBag
  , scenarioEncounterDeck = mempty
  , scenarioDiscard = mempty
  , scenarioResignedCardCodes = mempty
  , scenarioStoryCards = mempty
  }

instance Entity ScenarioAttrs where
  type EntityId ScenarioAttrs = ScenarioId
  type EntityAttrs ScenarioAttrs = ScenarioAttrs
  toId = scenarioId
  toAttrs = id

instance Named ScenarioAttrs where
  toName = scenarioName

instance TargetEntity ScenarioAttrs where
  toTarget = ScenarioTarget . toId
  isTarget ScenarioAttrs { scenarioId } (ScenarioTarget sid) =
    scenarioId == sid
  isTarget _ _ = False

instance SourceEntity ScenarioAttrs where
  toSource = ScenarioSource . toId
  isSource ScenarioAttrs { scenarioId } (ScenarioSource sid) =
    scenarioId == sid
  isSource _ _ = False
