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
  (preyL .~ Prey (NearestToEnemy (EnemyWithTrait Trait.Cultist <> NotEnemy (enemyIs Cards.narogath))))

instance (HasSet InvestigatorId env LocationId, HasSet ConnectedLocationId env LocationId) => HasModifiersFor Narogath where
  getModifiersFor _ (InvestigatorTarget iid) (Narogath a@EnemyAttrs {..}) = case enemyLocation of
    Nothing -> pure []
    Just loc -> do
      connectedLocationIds <- map unConnectedLocationId
        <$> getSetList loc
      iids <- concatMapM getSetList (loc : connectedLocationIds)
      pure $ toModifiers
        a
        [ CannotTakeAction (EnemyAction Parley [Cultist])
        | not enemyExhausted && iid `elem` iids
        ]
  getModifiersFor _ _ _ = pure []

instance (EnemyRunner env) => RunMessage Narogath where
  runMessage msg (Narogath attrs@EnemyAttrs {..}) = case msg of
    EnemySpawnEngagedWithPrey eid | eid == enemyId -> do
      playerCount <- unPlayerCount <$> getCount ()
      Narogath
        <$> runMessage msg (attrs & healthL %~ fmap (+ (3 * playerCount)))
    _ -> Narogath <$> runMessage msg attrs
