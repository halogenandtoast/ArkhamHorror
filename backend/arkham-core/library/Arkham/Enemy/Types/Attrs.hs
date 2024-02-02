module Arkham.Enemy.Types.Attrs where

import Arkham.Prelude

import Arkham.Card
import Arkham.ChaosToken
import Arkham.DamageEffect
import Arkham.GameValue
import Arkham.Id
import Arkham.Json
import Arkham.Key
import Arkham.Matcher
import Arkham.Modifier (Modifier)
import Arkham.Placement
import Arkham.Source
import Arkham.Spawn
import Arkham.Strategy
import Arkham.Token

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
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NoThunks, NFData)

enemyDamage :: EnemyAttrs -> Int
enemyDamage = countTokens Damage . enemyTokens

enemyClues :: EnemyAttrs -> Int
enemyClues = countTokens Clue . enemyTokens

enemyDoom :: EnemyAttrs -> Int
enemyDoom = countTokens Doom . enemyTokens

enemyResources :: EnemyAttrs -> Int
enemyResources = countTokens Resource . enemyTokens

instance ToJSON EnemyAttrs where
  toJSON = genericToJSON $ aesonOptions $ Just "enemy"
  toEncoding = genericToEncoding $ aesonOptions $ Just "enemy"

instance AsId EnemyAttrs where
  type IdOf EnemyAttrs = EnemyId
  asId = enemyId

instance FromJSON EnemyAttrs where
  parseJSON = withObject "EnemyAttrs" $ \o -> do
    enemyId <- o .: "id"
    enemyCardId <- o .: "cardId"
    enemyCardCode <- o .: "cardCode"
    enemyOriginalCardCode <- o .: "originalCardCode"
    enemyPlacement <- o .: "placement"
    enemyFight <- o .: "fight"
    enemyHealth <- o .: "health"
    enemyEvade <- o .: "evade"
    enemyAssignedDamage <- o .: "assignedDamage"
    enemyHealthDamage <- o .: "healthDamage"
    enemySanityDamage <- o .: "sanityDamage"
    enemyPrey <- o .: "prey"
    enemyModifiers <- o .: "modifiers"
    enemyExhausted <- o .: "exhausted"
    enemyTokens <- o .: "tokens"
    enemySpawnAt <- o .: "spawnAt"
    enemySurgeIfUnableToSpawn <- o .: "surgeIfUnableToSpawn"
    enemyAsSelfLocation <- o .: "asSelfLocation"
    enemyMovedFromHunterKeyword <- o .: "movedFromHunterKeyword"
    enemyDamageStrategy <- o .: "damageStrategy"
    enemyBearer <- o .: "bearer"
    enemySealedChaosTokens <- o .: "sealedChaosTokens"
    enemyKeys <- o .: "keys"
    enemySpawnedBy <- o .: "spawnedBy"
    enemyDiscardedBy <- o .:? "discardedBy" .!= Nothing
    pure EnemyAttrs {..}

instance Be EnemyAttrs EnemyMatcher where
  be = EnemyWithId . enemyId

instance HasCardCode EnemyAttrs where
  toCardCode = enemyCardCode

enemyReady :: EnemyAttrs -> Bool
enemyReady = not . enemyExhausted
