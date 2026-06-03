{-# LANGUAGE TemplateHaskell #-}

module Arkham.Message.Engage where

import Arkham.Id
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Source
import Arkham.Target
import Data.Aeson.TH

-- | Messages dealing with investigator-enemy engagement state.
data EngageMessage
  = EngageEnemy_ InvestigatorId EnemyId (Maybe Target) Bool
  | DisengageEnemy_ InvestigatorId EnemyId
  | DisengageEnemyFromAll_ EnemyId
  | ChooseEngageEnemy_ InvestigatorId Source (Maybe Target) EnemyMatcher Bool
  | CheckEnemyEngagement_ InvestigatorId
  | EnemyCheckEngagement_ EnemyId
  | EnemyEngageInvestigator_ EnemyId InvestigatorId
  deriving stock (Show, Ord, Eq, Data)

$(deriveJSON defaultOptions ''EngageMessage)
