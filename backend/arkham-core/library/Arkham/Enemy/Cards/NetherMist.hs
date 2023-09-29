module Arkham.Enemy.Cards.NetherMist (
  netherMist,
  NetherMist (..),
) where

import Arkham.Prelude

import Arkham.Attack
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher

newtype NetherMist = NetherMist EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasAbilities NetherMist where
  getAbilities (NetherMist a) =
    withBaseAbilities
      a
      [ haunted
          "Nether Mist attacks you."
          ( ProxySource
              (LocationMatcherSource $ LocationWithEnemy $ EnemyWithId $ toId a)
              (toSource a)
          )
          1
      ]

netherMist :: EnemyCard NetherMist
netherMist =
  enemyWith
    NetherMist
    Cards.netherMist
    (3, Static 4, 3)
    (1, 1)
    (preyL .~ Prey (InvestigatorAt $ LocationWithMostClues Anywhere))

instance RunMessage NetherMist where
  runMessage msg e@(NetherMist attrs) = case msg of
    UseCardAbility iid (isProxySource attrs -> True) 1 _ _ -> do
      push $ EnemyAttack $ enemyAttack (toId attrs) attrs iid
      pure e
    _ -> NetherMist <$> runMessage msg attrs
