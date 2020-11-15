{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.Narogath where

import Arkham.Import hiding (Cultist)

import Arkham.Types.Action
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Trait
import qualified Arkham.Types.Trait as Trait

newtype Narogath = Narogath Attrs
  deriving newtype (Show, ToJSON, FromJSON)

narogath :: EnemyId -> Narogath
narogath uuid =
  Narogath
    $ baseAttrs uuid "50026b"
    $ (healthDamage .~ 1)
    . (sanityDamage .~ 2)
    . (fight .~ 3)
    . (health .~ Static 4)
    . (evade .~ 3)
    . (prey .~ NearestToEnemyWithTrait Trait.Cultist)

instance (HasSet InvestigatorId LocationId env, HasSet ConnectedLocationId LocationId env) => HasModifiersFor env Narogath where
  getModifiersFor _ (InvestigatorTarget iid) (Narogath Attrs {..}) = do
    connectedLocationIds <-
      asks $ map unConnectedLocationId . setToList . getSet enemyLocation
    iids <- concat <$> for
      (enemyLocation : connectedLocationIds)
      (\locationId -> asks $ setToList . getSet locationId)
    pure
      [ CannotTakeAction (EnemyAction Parley [Cultist])
      | not enemyExhausted && iid `elem` iids
      ]
  getModifiersFor _ _ _ = pure []

instance HasModifiers env Narogath where
  getModifiers _ (Narogath Attrs {..}) =
    pure . concat . toList $ enemyModifiers

instance ActionRunner env => HasActions env Narogath where
  getActions i window (Narogath attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env Narogath where
  runMessage msg (Narogath attrs@Attrs {..}) = case msg of
    EnemySpawnEngagedWithPrey eid | eid == enemyId -> do
      playerCount <- unPlayerCount <$> asks (getCount ())
      Narogath <$> runMessage msg (attrs & health %~ fmap (+ (3 * playerCount)))
    _ -> Narogath <$> runMessage msg attrs
