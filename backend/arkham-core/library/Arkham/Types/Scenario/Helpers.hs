module Arkham.Types.Scenario.Helpers where

import Arkham.Import

import Arkham.Types.EncounterSet
import System.Random.Shuffle

getXp :: (HasCount XPCount () env, MonadReader env m) => m Int
getXp = asks $ unXPCount . getCount ()

getLeadInvestigatorId
  :: (HasId LeadInvestigatorId () env, MonadReader env m) => m InvestigatorId
getLeadInvestigatorId = asks $ unLeadInvestigatorId . getId ()

getInvestigatorIds
  :: (HasSet InvestigatorId () env, MonadReader env m) => m [InvestigatorId]
getInvestigatorIds = asks $ setToList . getSet ()

getPlayerCount :: (HasCount PlayerCount () env, MonadReader env m) => m Int
getPlayerCount = asks $ unPlayerCount . getCount ()

buildEncounterDeck :: MonadIO m => [EncounterSet] -> m [EncounterCard]
buildEncounterDeck = buildEncounterDeckWith id

buildEncounterDeckWith
  :: MonadIO m
  => ([EncounterCard] -> [EncounterCard])
  -> [EncounterSet]
  -> m [EncounterCard]
buildEncounterDeckWith f encounterSets =
  liftIO $ shuffleM . f . concat =<< traverse gatherEncounterSet encounterSets
