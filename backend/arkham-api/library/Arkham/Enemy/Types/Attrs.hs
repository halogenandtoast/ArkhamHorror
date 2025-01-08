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
  , enemyDelayEngagement :: Bool
  , enemyCardsUnderneath :: [Card]
  }
  deriving stock (Show, Eq)

instance HasField "id" EnemyAttrs EnemyId where
  getField = enemyId

instance HasField "sanityDamage" EnemyAttrs Int where
  getField = enemySanityDamage

instance HasField "keys" EnemyAttrs (Set ArkhamKey) where
  getField = enemyKeys

instance HasField "cardId" EnemyAttrs CardId where
  getField = enemyCardId

instance HasField "meta" EnemyAttrs Value where
  getField = enemyMeta

instance HasField "placement" EnemyAttrs Placement where
  getField = enemyPlacement

instance HasField "ready" EnemyAttrs Bool where
  getField = not . enemyExhausted

instance HasField "exhausted" EnemyAttrs Bool where
  getField = enemyExhausted

instance HasField "tokens" EnemyAttrs Tokens where
  getField = enemyTokens

instance HasField "token" EnemyAttrs (Token -> Int) where
  getField a tType = countTokens tType a.tokens

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

$(deriveToJSON (aesonOptions $ Just "enemy") ''EnemyAttrs)

instance FromJSON EnemyAttrs where
  parseJSON = withObject "EnemyAttrs" $ \v -> do
    enemyId <- v .: "id"
    enemyCardId <- v .: "cardId"
    enemyCardCode <- v .: "cardCode"
    enemyOriginalCardCode <- v .: "originalCardCode"
    enemyPlacement <- v .: "placement"
    enemyFight <- v .:? "fight"
    enemyHealth <- v .:? "health"
    enemyEvade <- v .:? "evade"
    enemyAssignedDamage <- v .: "assignedDamage"
    enemyHealthDamage <- v .: "healthDamage"
    enemySanityDamage <- v .: "sanityDamage"
    enemyPrey <- v .: "prey"
    enemyModifiers <- v .: "modifiers"
    enemyExhausted <- v .: "exhausted"
    enemyTokens <- v .: "tokens"
    enemySpawnAt <- v .:? "spawnAt"
    enemySurgeIfUnableToSpawn <- v .: "surgeIfUnableToSpawn"
    enemyAsSelfLocation <- v .:? "asSelfLocation"
    enemyMovedFromHunterKeyword <- v .: "movedFromHunterKeyword"
    enemyDamageStrategy <- v .: "damageStrategy"
    enemyBearer <- v .:? "bearer"
    enemySealedChaosTokens <- v .: "sealedChaosTokens"
    enemyKeys <- v .: "keys"
    enemySpawnedBy <- v .:? "spawnedBy"
    enemyDiscardedBy <- v .:? "discardedBy"
    enemyDefeated <- v .: "defeated"
    enemyAttacks <- v .: "attacks"
    enemyUnableToSpawn <- v .: "unableToSpawn"
    enemyMeta <- v .: "meta"
    enemyFlipped <- v .: "flipped"
    enemyAttacking <- v .:? "attacking"
    enemyDelayEngagement <- v .:? "delayEngagement" .!= False
    enemyCardsUnderneath <- v .:? "cardsUnderneath" .!= []
    return EnemyAttrs {..}
