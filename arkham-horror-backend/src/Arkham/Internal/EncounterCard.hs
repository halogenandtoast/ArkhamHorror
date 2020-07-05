module Arkham.Internal.EncounterCard where

import Arkham.Internal.Location
import Arkham.Types
import Arkham.Types.Card
import Arkham.Types.Enemy
import Arkham.Types.GameState
import Arkham.Types.Location
import Arkham.Types.Player
import Arkham.Types.Trait
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.UUID.V4
import Lens.Micro
import Lens.Micro.Platform ()
import Safe (fromJustNote)

data ArkhamEncounterCardType = EncounterEnemy | EncounterTreachery

data ArkhamEncounterCardInternal = ArkhamEncounterCardInternal
  { aeiType :: ArkhamEncounterCardType
  , aeiResolve :: forall m. MonadIO m => ArkhamPlayer -> ArkhamGameState -> m ArkhamGameState
  , aeiCardCode :: ArkhamCardCode
  , aeiName :: Text
  }

type PlayerCount = Int

data ArkhamEnemyInternal = ArkhamEnemyInternal
  { enemyCombat :: PlayerCount -> Int
  , enemyHealth :: PlayerCount -> Int
  , enemyAgility :: PlayerCount -> Int
  , enemyHealthDamage :: Int
  , enemySanityDamage :: Int
  , enemyVictory :: Maybe Int
  , enemyCardCode :: ArkhamCardCode
  , enemyIsHunter :: Bool
  , enemyTraits :: [ArkhamTrait]
  , enemyName :: Text
  }

toInternalEncounterCard :: ArkhamEncounterCard -> ArkhamEncounterCardInternal
toInternalEncounterCard c =
  fromJustNote ("Could not find encounter card with card code " <> show ccode)
    $ HashMap.lookup ccode encounterCardsInternal
  where ccode = aecCode c

toInternalEnemy :: ArkhamEnemy -> ArkhamEnemyInternal
toInternalEnemy e =
  fromJustNote ("Could not find enemy with card code " <> show ccode)
    $ HashMap.lookup ccode enemiesInternal
  where ccode = _enemyCardCode e

lookupEncounterCard :: MonadIO m => ArkhamCardCode -> m ArkhamEncounterCard
lookupEncounterCard ccode' =  toEncounterCard . fromJustNote "Could not find encounter card" $ HashMap.lookup ccode' encounterCardsInternal

toEncounterCard :: MonadIO m => ArkhamEncounterCardInternal -> m ArkhamEncounterCard
toEncounterCard ArkhamEncounterCardInternal {..} = do
  pure ArkhamEncounterCard
    { aecName = aeiName
    , aecCode = aeiCardCode
    , aecImage = 
      "https://arkhamdb.com/bundles/cards/"
      <> (unArkhamCardCode aeiCardCode)
      <> ".png"
    }

toEnemy :: MonadIO m => ArkhamEnemyInternal -> ArkhamGameState -> m ArkhamEnemy
toEnemy ArkhamEnemyInternal {..} _ = do
  enemyId <- liftIO nextRandom
  pure ArkhamEnemy
    { _enemyId = enemyId
    , _enemyCombat = enemyCombat 1
    , _enemyHealth = enemyHealth 1
    , _enemyDamage = 0
    , _enemyAgility = enemyAgility 1
    , _enemyHealthDamage = enemyHealthDamage
    , _enemySanityDamage = enemySanityDamage
    , _enemyVictory = enemyVictory
    , _enemyCardCode = enemyCardCode
    , _enemyIsHunter = enemyIsHunter
    , _enemyIsEngaged = False
    , _enemyImage =
      "https://arkhamdb.com/bundles/cards/"
      <> unpack (unArkhamCardCode enemyCardCode)
      <> ".png"
    , _enemyTraits = enemyTraits
    , _enemyFinishedAttacking = False
    }

spawnAt
  :: MonadIO m
  => ArkhamLocation
  -> ArkhamEnemyInternal
  -> ArkhamGameState
  -> m ArkhamGameState
spawnAt l e g = do
  enemy' <- toEnemy e g
  let
    investigators' =
      g ^. locations . at (alCardCode l) . _Just . to alInvestigators
    willEngage = not (null investigators')
    enemy'' = if willEngage then enemy' { _enemyIsEngaged = True } else enemy'
    engage =
      if willEngage then activePlayer . enemyIds %~ (HashSet.insert (_enemyId enemy'')) else id
  pure
    $ g
    & enemies
    %~ HashMap.insert (_enemyId enemy'') enemy''
    & locations
    . at (alCardCode l)
    . _Just
    . enemyIds
    %~ HashSet.insert (_enemyId enemy'')
    & engage

encounterCardsInternal :: HashMap ArkhamCardCode ArkhamEncounterCardInternal
encounterCardsInternal = HashMap.map enemy enemiesInternal

enemiesInternal :: HashMap ArkhamCardCode ArkhamEnemyInternal
enemiesInternal = HashMap.fromList
  $ map (\c -> (enemyCardCode c, c)) [fleshEater, icyGhoul, swarmOfRats]

enemy :: ArkhamEnemyInternal -> ArkhamEncounterCardInternal
enemy it = ArkhamEncounterCardInternal
  { aeiType = EncounterEnemy
  , aeiResolve = \p g -> spawnAt (locationFor p g) it g
  , aeiCardCode = enemyCardCode it
  , aeiName = enemyName it
  }

defaultEnemy :: ArkhamCardCode -> Text -> ArkhamEnemyInternal
defaultEnemy ccode name = ArkhamEnemyInternal
  { enemyCombat = const 1
  , enemyHealth = const 1
  , enemyAgility = const 1
  , enemyHealthDamage = 0
  , enemySanityDamage = 0
  , enemyVictory = Nothing
  , enemyCardCode = ccode
  , enemyIsHunter = False
  , enemyTraits = []
  , enemyName = name
  }

fleshEater :: ArkhamEnemyInternal
fleshEater = (defaultEnemy (ArkhamCardCode "01118") "Flesh-Eater")
  { enemyCombat = const 4
  , enemyHealth = const 4
  , enemyHealthDamage = 1
  , enemySanityDamage = 2
  , enemyVictory = Just 1
  , enemyTraits = [Humanoid, Monster, Ghoul]
  }

icyGhoul :: ArkhamEnemyInternal
icyGhoul = (defaultEnemy (ArkhamCardCode "01119") "Icy Ghoul")
  { enemyCombat = const 3
  , enemyHealth = const 4
  , enemyAgility = const 4
  , enemyHealthDamage = 2
  , enemySanityDamage = 1
  , enemyVictory = Just 1
  , enemyTraits = [Humanoid, Monster, Ghoul]
  }

swarmOfRats :: ArkhamEnemyInternal
swarmOfRats = (defaultEnemy (ArkhamCardCode "01159") "Swarm of Rats")
  { enemyAgility = const 3
  , enemyHealthDamage = 1
  , enemyIsHunter = True
  , enemyTraits = [Creature]
  }
