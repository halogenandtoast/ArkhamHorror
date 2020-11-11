{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.SwampLeech
  ( SwampLeech(..)
  , swampLeech
  )
where

import Arkham.Import
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Trait

newtype SwampLeech = SwampLeech Attrs
  deriving newtype (Show, ToJSON, FromJSON)

swampLeech :: EnemyId -> SwampLeech
swampLeech uuid =
  SwampLeech
    $ baseAttrs uuid "81023"
    $ (healthDamage .~ 1)
    . (fight .~ 4)
    . (health .~ Static 1)
    . (evade .~ 0)

instance HasModifiersFor env SwampLeech where
  getModifiersFor = noModifiersFor

instance HasModifiers env SwampLeech where
  getModifiers _ (SwampLeech Attrs {..}) =
    pure . concat . toList $ enemyModifiers

isEvade :: Message -> Bool
isEvade = \case
  EvadeEnemy{} -> True
  _ -> False

instance ActionRunner env => HasActions env SwampLeech where
  getActions i window (SwampLeech attrs) = do
    actions' <- getActions i window attrs
    pure $ filter (not . isEvade) actions'

instance EnemyRunner env => RunMessage env SwampLeech where
  runMessage msg e@(SwampLeech attrs@Attrs {..}) = case msg of
    InvestigatorDrawEnemy iid _ eid | eid == enemyId -> do
      bayouLocations <- asks $ setToList . getSet @LocationId [Bayou]
      e <$ spawnAtOneOf iid enemyId bayouLocations
    EnemyMove eid _ lid | eid == enemyId -> do
      bayouLocations <- asks $ setToList . getSet @LocationId [Bayou]
      e <$ when
        (lid `notElem` bayouLocations)
        (unshiftMessage $ Discard (EnemyTarget enemyId))
    _ -> SwampLeech <$> runMessage msg attrs
