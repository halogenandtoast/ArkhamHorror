module Arkham.Enemy.Cards.Narogath
  ( narogath
  , Narogath(..)
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Action
import Arkham.Classes
import Arkham.Enemy.Runner
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Modifier
import Arkham.Prey
import Arkham.Query
import Arkham.Target
import Arkham.Trait
import Arkham.Trait qualified as Trait

newtype Narogath = Narogath EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

narogath :: EnemyCard Narogath
narogath = enemyWith
  Narogath
  Cards.narogath
  (3, Static 4, 3)
  (1, 2)
  (preyL .~ NearestToEnemy (EnemyWithTrait Trait.Cultist <> NotEnemy (enemyIs Cards.narogath)))

instance (HasSet InvestigatorId env LocationId, HasSet ConnectedLocationId env LocationId) => HasModifiersFor env Narogath where
  getModifiersFor _ (InvestigatorTarget iid) (Narogath a@EnemyAttrs {..})
    | spawned a = do
      connectedLocationIds <- map unConnectedLocationId
        <$> getSetList enemyLocation
      iids <- concat <$> for (enemyLocation : connectedLocationIds) getSetList
      pure $ toModifiers
        a
        [ CannotTakeAction (EnemyAction Parley [Cultist])
        | not enemyExhausted && iid `elem` iids
        ]
  getModifiersFor _ _ _ = pure []

instance (EnemyRunner env) => RunMessage env Narogath where
  runMessage msg (Narogath attrs@EnemyAttrs {..}) = case msg of
    EnemySpawnEngagedWithPrey eid | eid == enemyId -> do
      playerCount <- unPlayerCount <$> getCount ()
      Narogath
        <$> runMessage msg (attrs & healthL %~ fmap (+ (3 * playerCount)))
    _ -> Narogath <$> runMessage msg attrs
