{-# LANGUAGE TemplateHaskell #-}

module Arkham.Enemy.Types.Attrs where

import Arkham.Attack.Types
import Arkham.Card
import Arkham.ChaosToken.Types
import Arkham.DamageEffect
import Arkham.GameValue
import Arkham.Id
import Arkham.Json
import Arkham.Key
import Arkham.Matcher
import Arkham.Modifier (Modifier)
import Arkham.Placement
import Arkham.Prelude
import Arkham.Source
import Arkham.Spawn
import Arkham.Strategy
import Arkham.Token
import Data.Aeson.TH
import GHC.Records

data UnableToSpawn = DiscardIfUnableToSpawn | ShuffleBackInIfUnableToSpawn
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data EnemyAttrs = EnemyAttrs
  { enemyId :: EnemyId
  , enemyCardId :: CardId
  , enemyCardCode :: CardCode
  , enemyOriginalCardCode :: CardCode
  , enemyPlacement :: Placement
  , enemyFight :: Maybe Int
  , enemyHealth :: Maybe GameValue
  , enemyEvade :: Maybe Int
  , enemyAssignedDamage :: Map Source DamageAssignment
  , enemyHealthDamage :: Int
  , enemySanityDamage :: Int
  , enemyPrey :: PreyMatcher
  , enemyModifiers :: Map Source [Modifier]
  , enemyExhausted :: Bool
  , enemyTokens :: Tokens
  , enemySpawnAt :: Maybe SpawnAt
  , enemySurgeIfUnableToSpawn :: Bool
  , enemyAsSelfLocation :: Maybe Text
  , enemyMovedFromHunterKeyword :: Bool
  , enemyDamageStrategy :: DamageStrategy
  , enemyBearer :: Maybe InvestigatorId
  , enemySealedChaosTokens :: [ChaosToken]
  , enemyKeys :: Set ArkhamKey
  , enemySpawnedBy :: Maybe InvestigatorId
  , enemyDiscardedBy :: Maybe InvestigatorId
  , enemyDefeated :: Bool
  , enemyAttacks :: InvestigatorMatcher
  , enemyUnableToSpawn :: UnableToSpawn
  , enemyMeta :: Value
  , enemyFlipped :: Bool
  , enemyAttacking :: Maybe EnemyAttackDetails
  }
  deriving stock (Show, Eq)

instance HasField "id" EnemyAttrs EnemyId where
  getField = enemyId

instance HasField "meta" EnemyAttrs Value where
  getField = enemyMeta

instance HasField "placement" EnemyAttrs Placement where
  getField = enemyPlacement

enemyDamage :: EnemyAttrs -> Int
enemyDamage = countTokens Damage . enemyTokens

enemyClues :: EnemyAttrs -> Int
enemyClues = countTokens Clue . enemyTokens

enemyDoom :: EnemyAttrs -> Int
enemyDoom = countTokens Doom . enemyTokens

enemyResources :: EnemyAttrs -> Int
enemyResources = countTokens Resource . enemyTokens

instance AsId EnemyAttrs where
  type IdOf EnemyAttrs = EnemyId
  asId = enemyId

instance Be EnemyAttrs EnemyMatcher where
  be = EnemyWithId . enemyId

instance HasCardCode EnemyAttrs where
  toCardCode = enemyCardCode

enemyReady :: EnemyAttrs -> Bool
enemyReady = not . enemyExhausted

$(deriveJSON (aesonOptions $ Just "enemy") ''EnemyAttrs)
