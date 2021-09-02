module Arkham.Types.Enemy.Cards.ServantOfManyMouths
  ( ServantOfManyMouths(..)
  , servantOfManyMouths
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (EnemyDefeated)
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing

newtype ServantOfManyMouths = ServantOfManyMouths EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
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

instance EnemyRunner env => RunMessage env ServantOfManyMouths where
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
                [DiscoverCluesAtLocation iid lid 1 Nothing]
            | lid <- locationsWithClues
            ]
          )
        )
    _ -> ServantOfManyMouths <$> runMessage msg attrs
