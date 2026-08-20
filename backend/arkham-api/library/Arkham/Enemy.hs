{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Enemy where

import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.DefeatedProxy (toDefeatedEnemyProxy)
import Arkham.Enemy.Enemies
import Arkham.Enemy.Runner
import Arkham.Helpers.Modifiers
import Arkham.Homebrew.Registry qualified as Registry
import Arkham.Matcher
import Arkham.Placement
import Arkham.Prelude

createEnemy :: (HasCallStack, IsCard a) => a -> EnemyId -> Enemy
createEnemy a eid = lookupEnemy (toCardCode a) eid (toCardId a)

instance RunMessage Enemy where
  runMessage (SendMessage target msg) e | e `is` target = runMessage msg e
  runMessage msg e@(Enemy x) = do
    -- we must check that an enemy exists when grabbing modifiers
    -- as some messages are not masked when targetting cards in the
    -- discard.
    case attr enemyPlacement e of
      OutOfGame _ -> case msg of
        ReturnLocationToGame {} -> Enemy <$> runMessage msg x
        _ -> pure e
      _ -> do
        -- See the matching comment in Arkham.Asset.Runner: cheap test first.
        modifiers' <- getModifiers (toTarget e)
        msg' <-
          if Blank `elem` modifiers'
            then do
              inPlay <- elem (toId e) <$> select AnyEnemy
              pure $ if inPlay then Blanked msg else msg
            else pure msg
        Enemy <$> runMessage msg' x

lookupEnemy :: HasCallStack => CardCode -> EnemyId -> CardId -> Enemy
lookupEnemy cardCode = case lookup cardCode allEnemies of
  Nothing -> error $ "Unknown enemy (lookupEnemy): " <> show cardCode <> "\n\n" <> prettyCallStack callStack
  Just (SomeEnemyCard a) -> \e c -> Enemy $ cbCardBuilder a c e

{- | Rebuild an 'Enemy' from the attrs recorded when it was defeated.

Unlike 'lookupEnemy' this tolerates a card code with no enemy builder, because
enemy-locations defeat as enemies and land in @ScenarioDefeatedEnemies@ too.
'lookupEnemy' keeps its error: it builds enemies at spawn, where an inert
fallback would mask a real bug.
-}
lookupDefeatedEnemy :: EnemyAttrs -> Enemy
lookupDefeatedEnemy a = case lookup (toCardCode a) allEnemies of
  Just (SomeEnemyCard b) -> overAttrs (const a) $ Enemy $ cbCardBuilder b (toCardId a) (toId a)
  Nothing -> toDefeatedEnemyProxy a

instance FromJSON Enemy where
  parseJSON = withObject "Enemy" $ \o -> do
    cCode <- o .: "cardCode"
    withEnemyCardCode cCode
      $ \(_ :: EnemyCard a) -> Enemy <$> parseJSON @a (Object o)

withEnemyCardCode
  :: CardCode -> (forall a. IsEnemy a => EnemyCard a -> r) -> r
withEnemyCardCode cCode f = case lookup cCode allEnemies of
  Nothing ->
    error $ "Unknown enemy (withEnemyCardCode): " <> show cCode <> "\n\n" <> prettyCallStack callStack
  Just (SomeEnemyCard a) -> f a

allEnemies :: Map CardCode SomeEnemyCard
allEnemies =
  (mapFromList (concatMap someEnemyCardCodes Registry.enemies) <>)
    $ mapFromList
    $ concatMap someEnemyCardCodes allEnemyCardBuilders
