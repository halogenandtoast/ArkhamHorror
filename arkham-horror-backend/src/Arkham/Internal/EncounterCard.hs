module Arkham.Internal.EncounterCard where

import Arkham.Internal.Investigator
import Arkham.Types
import Arkham.Types.Card
import Arkham.Types.Enemy
import Arkham.Types.GameState
import Arkham.Types.Investigator
import Arkham.Types.Location
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
import Data.UUID.V4
import Lens.Micro
import Lens.Micro.Platform ()
import Safe (fromJustNote)

data ArkhamEncounterCardType = EncounterEnemy | EncounterTreachery

data ArkhamEncounterCardInternal = ArkhamEncounterCardInternal
  { aeiType :: ArkhamEncounterCardType
  , aeiResolve :: forall m. MonadIO m => ArkhamInvestigator -> ArkhamGameState -> m ArkhamGameState
  , aeiCardCode :: ArkhamCardCode
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

toEnemy :: MonadIO m => ArkhamEnemyInternal -> ArkhamGameState -> m ArkhamEnemy
toEnemy ArkhamEnemyInternal {..} _ = do
  enemyId <- liftIO nextRandom
  pure ArkhamEnemy
    { _enemyId = enemyId
    , _enemyCombat = enemyCombat 1
    , _enemyHealth = enemyHealth 1
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
    investigators =
      g ^. locations . at (alCardCode l) . _Just . to alInvestigators
    willEngage = not (null investigators)
    enemy'' = if willEngage then enemy' { _enemyIsEngaged = True } else enemy'
    engage = if willEngage
      then \g' -> g' & player . enemyIds %~ (_enemyId enemy'' :)
      else id
  pure
    $ g
    & enemies
    %~ HashMap.insert (_enemyId enemy'') enemy''
    & locations
    . at (alCardCode l)
    . _Just
    . enemyIds
    %~ (_enemyId enemy'' :)
    & engage

encounterCardsInternal :: HashMap ArkhamCardCode ArkhamEncounterCardInternal
encounterCardsInternal = HashMap.map enemy enemiesInternal

enemiesInternal :: HashMap ArkhamCardCode ArkhamEnemyInternal
enemiesInternal = HashMap.fromList
  $ map (\c -> (enemyCardCode c, c)) [fleshEater, icyGhoul, swarmOfRats]

enemy :: ArkhamEnemyInternal -> ArkhamEncounterCardInternal
enemy it = ArkhamEncounterCardInternal
  { aeiType = EncounterEnemy
  , aeiResolve = \i g -> spawnAt (locationFor i g) it g
  , aeiCardCode = enemyCardCode it
  }

defaultEnemy :: ArkhamCardCode -> ArkhamEnemyInternal
defaultEnemy ccode = ArkhamEnemyInternal
  { enemyCombat = const 1
  , enemyHealth = const 1
  , enemyAgility = const 1
  , enemyHealthDamage = 0
  , enemySanityDamage = 0
  , enemyVictory = Nothing
  , enemyCardCode = ccode
  , enemyIsHunter = False
  }

fleshEater :: ArkhamEnemyInternal
fleshEater = (defaultEnemy $ ArkhamCardCode "01118")
  { enemyCombat = const 4
  , enemyHealth = const 4
  , enemyHealthDamage = 1
  , enemySanityDamage = 2
  , enemyVictory = Just 1
  }

icyGhoul :: ArkhamEnemyInternal
icyGhoul = (defaultEnemy $ ArkhamCardCode "01119")
  { enemyCombat = const 3
  , enemyHealth = const 4
  , enemyAgility = const 4
  , enemyHealthDamage = 2
  , enemySanityDamage = 1
  , enemyVictory = Just 1
  }

swarmOfRats :: ArkhamEnemyInternal
swarmOfRats = (defaultEnemy $ ArkhamCardCode "01159")
  { enemyAgility = const 3
  , enemyHealthDamage = 1
  , enemyIsHunter = True
  }
