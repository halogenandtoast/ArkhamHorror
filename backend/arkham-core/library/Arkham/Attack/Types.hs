{-# LANGUAGE TemplateHaskell #-}

module Arkham.Attack.Types where

import Arkham.Prelude

import Arkham.Id
import {-# SOURCE #-} Arkham.Message
import Arkham.Source
import Arkham.Strategy
import Arkham.Target
import Data.Aeson.TH
import GHC.Records

data EnemyAttackType = AttackOfOpportunity | RegularAttack | AlertAttack
  deriving stock (Show, Eq)

data EnemyAttackDetails = EnemyAttackDetails
  { attackTarget :: Target
  , attackOriginalTarget :: Target
  , attackEnemy :: EnemyId
  , attackType :: EnemyAttackType
  , attackDamageStrategy :: DamageStrategy
  , attackExhaustsEnemy :: Bool
  , attackSource :: Source
  , attackCanBeCanceled :: Bool
  , attackAfter :: [Message]
  }
  deriving stock (Show, Eq)

instance HasField "target" EnemyAttackDetails Target where
  getField = attackTarget

damageStrategyL :: Lens' EnemyAttackDetails DamageStrategy
damageStrategyL =
  lens attackDamageStrategy $ \m x -> m {attackDamageStrategy = x}

$(deriveJSON defaultOptions ''EnemyAttackType)

instance FromJSON EnemyAttackDetails where
  parseJSON = withObject "EnemyAttackDetails" $ \o -> do
    attackTarget <- o .: "attackTarget"
    attackOriginalTarget <- o .: "attackOriginalTarget" <|> pure attackTarget
    attackEnemy <- o .: "attackEnemy"
    attackType <- o .: "attackType"
    attackDamageStrategy <- o .: "attackDamageStrategy"
    attackExhaustsEnemy <- o .: "attackExhaustsEnemy"
    attackSource <- o .: "attackSource"
    attackCanBeCanceled <- o .: "attackCanBeCanceled"
    attackAfter <- o .:? "attackAfter" .!= []
    pure EnemyAttackDetails {..}

$(deriveToJSON defaultOptions ''EnemyAttackDetails)
