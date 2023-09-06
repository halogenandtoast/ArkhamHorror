module Arkham.Enemy.Cards.AnetteMasonReincarnatedEvil
  ( anetteMasonReincarnatedEvil
  , AnetteMasonReincarnatedEvil(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype AnetteMasonReincarnatedEvil = AnetteMasonReincarnatedEvil EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

anetteMasonReincarnatedEvil :: EnemyCard AnetteMasonReincarnatedEvil
anetteMasonReincarnatedEvil = enemy AnetteMasonReincarnatedEvil Cards.anetteMasonReincarnatedEvil (5, PerPlayer 6, 3) (3, 1)

instance RunMessage AnetteMasonReincarnatedEvil where
  runMessage msg (AnetteMasonReincarnatedEvil attrs) =
    AnetteMasonReincarnatedEvil <$> runMessage msg attrs
