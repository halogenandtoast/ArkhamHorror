module Arkham.Enemy.Cards.ServantOfManyMouths (
  ServantOfManyMouths (..),
  servantOfManyMouths,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Discover
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message qualified as Msg

newtype ServantOfManyMouths = ServantOfManyMouths EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

servantOfManyMouths :: EnemyCard ServantOfManyMouths
servantOfManyMouths =
  enemyWith
    ServantOfManyMouths
    Cards.servantOfManyMouths
    (3, Static 2, 1)
    (2, 0)
    (spawnAtL ?~ SpawnAt EmptyLocation)

instance HasAbilities ServantOfManyMouths where
  getAbilities (ServantOfManyMouths attrs) =
    withBaseAbilities
      attrs
      [ restrictedAbility
          attrs
          1
          (LocationExists LocationWithAnyClues <> CanDiscoverCluesAt Anywhere)
          $ freeReaction
          $ EnemyDefeated #after You ByAny
          $ EnemyWithId attrs.id
      ]

instance RunMessage ServantOfManyMouths where
  runMessage msg e@(ServantOfManyMouths attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      locationsWithClues <- select LocationWithAnyClues
      player <- getPlayer iid
      unless (null locationsWithClues) do
        push
          $ chooseOne
            player
            [ targetLabel
              lid
              [Msg.DiscoverClues iid $ discover lid (toAbilitySource attrs 1) 1]
            | lid <- locationsWithClues
            ]
      pure e
    _ -> ServantOfManyMouths <$> runMessage msg attrs
