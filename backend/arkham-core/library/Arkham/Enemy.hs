{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.Enemy
  ( module Arkham.Enemy
  ) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Enemies
import Arkham.Enemy.Runner
import Arkham.Id
import Arkham.Matcher
import Arkham.Message

createEnemy :: (HasCallStack, IsCard a) => a -> Enemy
createEnemy a = lookupEnemy (toCardCode a) (EnemyId $ toCardId a)

instance RunMessage Enemy where
  runMessage msg e@(Enemy x) = do
    -- we must check that an enemy exists when grabbing modifiers
    -- as some messages are not masked when targetting cards in the
    -- discard.
    allEnemyIds <- select AnyEnemy
    modifiers' <- if toId e `member` allEnemyIds
      then getModifiers (toTarget e)
      else pure []
    let msg' = if Blank `elem` modifiers' then Blanked msg else msg
    Enemy <$> runMessage msg' x

lookupEnemy :: HasCallStack => CardCode -> (EnemyId -> Enemy)
lookupEnemy cardCode = case lookup cardCode allEnemies of
  Nothing -> error $ "Unknown enemy: " <> show cardCode
  Just (SomeEnemyCard a) -> Enemy <$> cbCardBuilder a

instance FromJSON Enemy where
  parseJSON v = flip (withObject "Enemy") v $ \o -> do
    cCode :: CardCode <- o .: "cardCode"
    withEnemyCardCode cCode $ \(_ :: EnemyCard a) -> Enemy <$> parseJSON @a v

withEnemyCardCode
  :: CardCode
  -> (forall a. IsEnemy a => EnemyCard a -> r)
  -> r
withEnemyCardCode cCode f =
  case lookup cCode allEnemies of
    Nothing -> error $ "Unknown enemy: " <> show cCode
    Just (SomeEnemyCard a) -> f a

allEnemies :: HashMap CardCode SomeEnemyCard
allEnemies = mapFromList $ map
  (toFst someEnemyCardCode)
  [ -- Night of the Zealot
  -- weakness
    SomeEnemyCard mobEnforcer
  , SomeEnemyCard silverTwilightAcolyte
  , SomeEnemyCard stubbornDetective
  -- The Gathering
  , SomeEnemyCard ghoulPriest
  , SomeEnemyCard fleshEater
  , SomeEnemyCard icyGhoul
  -- Rats
  , SomeEnemyCard swarmOfRats
  -- Ghouls
  , SomeEnemyCard ghoulMinion
  , SomeEnemyCard ravenousGhoul
  ]
