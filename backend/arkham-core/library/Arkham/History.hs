{-# LANGUAGE TemplateHaskell #-}

module Arkham.History (module Arkham.History, module Arkham.History.Types) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Enemy.Types.Attrs
import Arkham.History.Types
import Arkham.Id
import Arkham.SkillTestResult
import Arkham.SkillType
import Arkham.Target
import Control.Monad.Fail (fail)
import Data.Aeson.TH
import Data.Data
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

data DefeatedEnemyAttrs = DefeatedEnemyAttrs
  { defeatedEnemyAttrs :: EnemyAttrs
  , defeatedEnemyHealth :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data History = History
  { historyTreacheriesDrawn :: [CardCode]
  , historyDealtDamageTo :: [Target]
  , historyEnemiesDefeated :: [DefeatedEnemyAttrs]
  , historyMoved :: Bool
  , historyLocationsSuccessfullyInvestigated :: Set LocationId
  , historySuccessfulExplore :: Bool
  , historyActionsCompleted :: Int
  , historySkillTestsPerformed :: [([SkillType], SkillTestResult)]
  , historyPlayedCards :: [Card]
  , historyCluesDiscovered :: Map LocationId Int
  }
  deriving stock (Show, Eq)

data HistoryField k where
  HistoryTreacheriesDrawn :: HistoryField [CardCode]
  HistoryDealtDamageTo :: HistoryField [Target]
  HistoryEnemiesDefeated :: HistoryField [DefeatedEnemyAttrs]
  HistoryMoved :: HistoryField Bool
  HistoryLocationsSuccessfullyInvestigated :: HistoryField (Set LocationId)
  HistorySuccessfulExplore :: HistoryField Bool
  HistoryActionsCompleted :: HistoryField Int
  HistorySkillTestsPerformed :: HistoryField [([SkillType], SkillTestResult)]
  HistoryPlayedCards :: HistoryField [Card]
  HistoryCluesDiscovered :: HistoryField (Map LocationId Int)

deriving stock instance Show (HistoryField k)
deriving stock instance Eq (HistoryField k)

viewHistoryField :: HistoryField k -> History -> k
viewHistoryField = \case
  HistoryTreacheriesDrawn -> historyTreacheriesDrawn
  HistoryDealtDamageTo -> historyDealtDamageTo
  HistoryEnemiesDefeated -> historyEnemiesDefeated
  HistoryMoved -> historyMoved
  HistoryLocationsSuccessfullyInvestigated -> historyLocationsSuccessfullyInvestigated
  HistorySuccessfulExplore -> historySuccessfulExplore
  HistoryActionsCompleted -> historyActionsCompleted
  HistorySkillTestsPerformed -> historySkillTestsPerformed
  HistoryPlayedCards -> historyPlayedCards
  HistoryCluesDiscovered -> historyCluesDiscovered

instance ToJSON (HistoryField k) where
  toJSON = toJSON . show

data SomeHistoryField where
  SomeHistoryField
    :: (Typeable k, Show k, Eq k, ToJSON k, FromJSON k) => HistoryField k -> SomeHistoryField

instance FromJSON SomeHistoryField where
  parseJSON = withText "SomeHistoryField" $ \t -> case t of
    "HistoryTreacheriesDrawn" -> pure $ SomeHistoryField HistoryTreacheriesDrawn
    "HistoryDealtDamageTo" -> pure $ SomeHistoryField HistoryDealtDamageTo
    "HistoryEnemiesDefeated" -> pure $ SomeHistoryField HistoryEnemiesDefeated
    "HistoryMoved" -> pure $ SomeHistoryField HistoryMoved
    "HistoryLocationsSuccessfullyInvestigated" -> pure $ SomeHistoryField HistoryLocationsSuccessfullyInvestigated
    "HistorySuccessfulExplore" -> pure $ SomeHistoryField HistorySuccessfulExplore
    "HistoryActionsCompleted" -> pure $ SomeHistoryField HistoryActionsCompleted
    "HistorySkillTestsPerformed" -> pure $ SomeHistoryField HistorySkillTestsPerformed
    "HistoryPlayedCards" -> pure $ SomeHistoryField HistoryPlayedCards
    "HistoryCluesDiscovered" -> pure $ SomeHistoryField HistoryCluesDiscovered
    _ -> fail $ "Invalid HistoryField: " <> T.unpack t

data HistoryItem where
  HistoryItem :: (Show k, Eq k, ToJSON k, Typeable k) => HistoryField k -> k -> HistoryItem

deriving stock instance Show HistoryItem

instance Data HistoryItem where
  gunfold _ _ _ = error "gunfold(HistoryItem)"
  toConstr _ = error "toConstr(HistoryItem)"
  dataTypeOf _ = error "dataTypeOf(HistoryItem)"

instance Eq HistoryItem where
  (HistoryItem (fld1 :: HistoryField k1) k1) == (HistoryItem (fld2 :: HistoryField k2) k2) = case eqT @k1 @k2 of
    Just Refl -> fld1 == fld2 && k1 == k2
    Nothing -> False

instance ToJSON HistoryItem where
  toJSON (HistoryItem fld k) = object ["field" .= fld, "value" .= k]

instance FromJSON HistoryItem where
  parseJSON = withObject "HistoryItem" $ \o -> do
    sfld <- o .: "field"
    case sfld of
      SomeHistoryField (fld :: HistoryField k) -> do
        k <- o .: "value"
        pure $ HistoryItem fld k

insertHistoryItem :: HistoryItem -> History -> History
insertHistoryItem (HistoryItem fld k) h =
  case fld of
    HistoryTreacheriesDrawn -> h {historyTreacheriesDrawn = nub $ historyTreacheriesDrawn h <> k}
    HistoryDealtDamageTo -> h {historyDealtDamageTo = nub $ historyDealtDamageTo h <> k}
    HistoryEnemiesDefeated -> h {historyEnemiesDefeated = nub $ historyEnemiesDefeated h <> k}
    HistoryMoved -> h {historyMoved = historyMoved h || k}
    HistoryLocationsSuccessfullyInvestigated ->
      h
        { historyLocationsSuccessfullyInvestigated =
            historyLocationsSuccessfullyInvestigated h <> k
        }
    HistorySuccessfulExplore -> h {historySuccessfulExplore = historySuccessfulExplore h || k}
    HistoryActionsCompleted -> h {historyActionsCompleted = historyActionsCompleted h + k}
    HistorySkillTestsPerformed -> h {historySkillTestsPerformed = historySkillTestsPerformed h <> k}
    HistoryPlayedCards -> h {historyPlayedCards = nub $ historyPlayedCards h <> k}
    HistoryCluesDiscovered -> h {historyCluesDiscovered = Map.unionWith (+) (historyCluesDiscovered h) k}

instance Semigroup History where
  h <> g =
    History
      { historyTreacheriesDrawn =
          historyTreacheriesDrawn h
            <> historyTreacheriesDrawn g
      , historyDealtDamageTo = historyDealtDamageTo h <> historyDealtDamageTo g
      , historyEnemiesDefeated =
          historyEnemiesDefeated h
            <> historyEnemiesDefeated g
      , historyMoved = historyMoved h || historyMoved g
      , historyLocationsSuccessfullyInvestigated =
          historyLocationsSuccessfullyInvestigated h
            <> historyLocationsSuccessfullyInvestigated g
      , historySuccessfulExplore =
          historySuccessfulExplore h
            || historySuccessfulExplore g
      , historyActionsCompleted =
          historyActionsCompleted h
            + historyActionsCompleted g
      , historySkillTestsPerformed =
          historySkillTestsPerformed h
            <> historySkillTestsPerformed g
      , historyPlayedCards = historyPlayedCards h <> historyPlayedCards g
      , historyCluesDiscovered = Map.unionWith (+) (historyCluesDiscovered h) (historyCluesDiscovered g)
      }

instance Monoid History where
  mempty = History [] [] [] False mempty False 0 [] [] mempty

insertHistory
  :: InvestigatorId
  -> HistoryItem
  -> Map InvestigatorId History
  -> Map InvestigatorId History
insertHistory iid histitem = Map.alter (Just . insertHistoryItem histitem . fromMaybe mempty) iid

$(deriveJSON defaultOptions ''History)
