module Arkham.Enemy.Cards.Acolyte
  ( Acolyte(..)
  , acolyte
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Classes
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing

newtype Acolyte = Acolyte EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

acolyte :: EnemyCard Acolyte
acolyte = enemyWith
  Acolyte
  Cards.acolyte
  (3, Static 1, 2)
  (1, 0)
  (spawnAtL ?~ EmptyLocation)

instance HasAbilities Acolyte where
  getAbilities (Acolyte a) = withBaseAbilities
    a
    [ mkAbility a 1
      $ ForcedAbility
      $ EnemySpawns Timing.After Anywhere
      $ EnemyWithId
      $ toId a
    ]

instance RunMessage Acolyte where
  runMessage msg e@(Acolyte attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      e <$ push (PlaceDoom (toTarget attrs) 1)
    _ -> Acolyte <$> runMessage msg attrs
