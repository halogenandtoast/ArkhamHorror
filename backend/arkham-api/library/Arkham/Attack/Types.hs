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

data EnemyAttackType = AttackOfOpportunity | RegularAttack | AlertAttack | RetaliateAttack
  deriving stock (Show, Eq, Data)

data AttackTarget = SingleAttackTarget Target | MassiveAttackTargets [Target]
  deriving stock (Show, Eq, Data)

data EnemyAttackDetails = EnemyAttackDetails
  { attackTarget :: AttackTarget
  , attackOriginalTarget :: AttackTarget
  , attackEnemy :: EnemyId
  , attackType :: EnemyAttackType
  , attackDamageStrategy :: DamageStrategy
  , attackExhaustsEnemy :: Bool
  , attackSource :: Source
  , attackCanBeCanceled :: Bool
  , attackAfter :: [Message]
  , attackDamaged :: Map Target (Int, Int)
  , attackDealDamage :: Bool
  , attackDespiteExhausted :: Bool
  }
  deriving stock (Show, Eq, Data)

instance HasField "despiteExhausted" EnemyAttackDetails Bool where
  getField = attackDespiteExhausted

instance HasField "kind" EnemyAttackDetails EnemyAttackType where
  getField = attackType

instance HasField "target" EnemyAttackDetails AttackTarget where
  getField = attackTarget

instance HasField "investigator" EnemyAttackDetails (Maybe InvestigatorId) where
  getField x = case attackTarget x of
    SingleAttackTarget a -> preview _InvestigatorTarget a
    _ -> Nothing

instance HasField "singleTarget" EnemyAttackDetails (Maybe Target) where
  getField x = case attackTarget x of
    SingleAttackTarget a -> Just a
    _ -> Nothing

instance HasField "targets" EnemyAttackDetails [Target] where
  getField x = case attackTarget x of
    SingleAttackTarget target -> [target]
    MassiveAttackTargets targets -> targets

instance HasField "source" EnemyAttackDetails Source where
  getField = attackSource

instance HasField "enemy" EnemyAttackDetails EnemyId where
  getField = attackEnemy

instance HasField "strategy" EnemyAttackDetails DamageStrategy where
  getField = attackDamageStrategy

instance HasField "damaged" EnemyAttackDetails (Map Target (Int, Int)) where
  getField = attackDamaged

instance HasField "canBeCanceled" EnemyAttackDetails Bool where
  getField = attackCanBeCanceled

damageStrategyL :: Lens' EnemyAttackDetails DamageStrategy
damageStrategyL = lens attackDamageStrategy $ \m x -> m {attackDamageStrategy = x}

damagedL :: Lens' EnemyAttackDetails (Map Target (Int, Int))
damagedL = lens attackDamaged $ \m x -> m {attackDamaged = x}

mconcat
  [ deriveJSON defaultOptions ''EnemyAttackType
  , deriveJSON defaultOptions ''AttackTarget
  , deriveToJSON defaultOptions ''EnemyAttackDetails
  ]

instance FromJSON EnemyAttackDetails where
  parseJSON = withObject "EnemyAttackDetails" $ \o -> do
    attackTarget <- o .: "attackTarget" <|> (SingleAttackTarget <$> o .: "attackTarget")
    attackOriginalTarget <- o .: "attackOriginalTarget" <|> (SingleAttackTarget <$> o .: "attackOriginalTarget")
    attackEnemy <- o .: "attackEnemy"
    attackType <- o .: "attackType"
    attackDamageStrategy <- o .: "attackDamageStrategy"
    attackExhaustsEnemy <- o .: "attackExhaustsEnemy"
    attackSource <- o .: "attackSource"
    attackCanBeCanceled <- o .: "attackCanBeCanceled"
    attackAfter <- o .: "attackAfter"
    attackDamaged <- o .: "attackDamaged"
    attackDealDamage <- o .:? "attackDealDamage" .!= True
    attackDespiteExhausted <- o .:? "attackDespiteExhausted" .!= False
    pure $ EnemyAttackDetails {..}
