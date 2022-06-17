module Arkham.Enemy.Cards.StubbornDetective
  ( StubbornDetective(..)
  , stubbornDetective
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Classes
import Arkham.Enemy.Runner
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Target

newtype StubbornDetective = StubbornDetective EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

stubbornDetective :: EnemyCard StubbornDetective
stubbornDetective = enemyWith
  StubbornDetective
  Cards.stubbornDetective
  (3, Static 2, 2)
  (1, 0)
  (\a -> a & preyL .~ BearerOf (toId a))

instance HasModifiersFor StubbornDetective where
  getModifiersFor _ (InvestigatorTarget iid) (StubbornDetective a@EnemyAttrs {..})
    | spawned a
    = do
      locationId <- getJustLocation iid
      pure $ toModifiers a [ Blank | Just locationId == enemyLocation ]
  getModifiersFor _ _ _ = pure []

instance RunMessage StubbornDetective where
  runMessage msg (StubbornDetective attrs) =
    StubbornDetective <$> runMessage msg attrs
