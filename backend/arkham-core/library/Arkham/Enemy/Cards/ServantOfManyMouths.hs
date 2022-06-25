module Arkham.Enemy.Cards.ServantOfManyMouths
  ( ServantOfManyMouths(..)
  , servantOfManyMouths
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message hiding (EnemyDefeated)
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype ServantOfManyMouths = ServantOfManyMouths EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

servantOfManyMouths :: EnemyCard ServantOfManyMouths
servantOfManyMouths = enemyWith
  ServantOfManyMouths
  Cards.servantOfManyMouths
  (3, Static 2, 1)
  (2, 0)
  (spawnAtL ?~ EmptyLocation)

instance HasAbilities ServantOfManyMouths where
  getAbilities (ServantOfManyMouths attrs) = withBaseAbilities
    attrs
    [ restrictedAbility
        attrs
        1
        (LocationExists LocationWithAnyClues <> CanDiscoverClues)
        (ReactionAbility
          (EnemyDefeated Timing.After You $ EnemyWithId $ toId attrs)
          Free
        )
    ]

instance RunMessage ServantOfManyMouths where
  runMessage msg e@(ServantOfManyMouths attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      locationsWithClues <- selectList LocationWithAnyClues
      e <$ unless
        (null locationsWithClues)
        (push
          (chooseOne
            iid
            [ TargetLabel
                (LocationTarget lid)
                [InvestigatorDiscoverClues iid lid 1 Nothing]
            | lid <- locationsWithClues
            ]
          )
        )
    _ -> ServantOfManyMouths <$> runMessage msg attrs
