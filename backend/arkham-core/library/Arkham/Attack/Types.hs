{-# LANGUAGE TemplateHaskell #-}

module Arkham.Attack.Types where

import Arkham.Id
import {-# SOURCE #-} Arkham.Message
import Arkham.Prelude
import Arkham.Source
import Arkham.Strategy
import Arkham.Target
import Data.Aeson.TH
import GHC.Records

data EnemyAttackType = AttackOfOpportunity | RegularAttack | AlertAttack
  deriving stock (Show, Eq, Data)

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
  , attackDamaged :: Map Target (Int, Int)
  }
  deriving stock (Show, Eq, Data)

instance HasField "target" EnemyAttackDetails Target where
  getField = attackTarget

instance HasField "enemy" EnemyAttackDetails EnemyId where
  getField = attackEnemy

instance HasField "strategy" EnemyAttackDetails DamageStrategy where
  getField = attackDamageStrategy

instance HasField "damaged" EnemyAttackDetails (Map Target (Int, Int)) where
  getField = attackDamaged

damageStrategyL :: Lens' EnemyAttackDetails DamageStrategy
damageStrategyL = lens attackDamageStrategy $ \m x -> m {attackDamageStrategy = x}

damagedL :: Lens' EnemyAttackDetails (Map Target (Int, Int))
damagedL = lens attackDamaged $ \m x -> m {attackDamaged = x}

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
    attackDamaged <- o .:? "attackDamaged" .!= mempty
    pure EnemyAttackDetails {..}

$(deriveToJSON defaultOptions ''EnemyAttackDetails)
