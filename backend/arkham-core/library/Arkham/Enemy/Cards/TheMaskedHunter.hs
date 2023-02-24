module Arkham.Enemy.Cards.TheMaskedHunter
  ( TheMaskedHunter(..)
  , theMaskedHunter
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher

newtype TheMaskedHunter = TheMaskedHunter EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theMaskedHunter :: EnemyCard TheMaskedHunter
theMaskedHunter = enemyWith
  TheMaskedHunter
  Cards.theMaskedHunter
  (4, Static 4, 2)
  (2, 1)
  (preyL .~ Prey MostClues)

instance HasModifiersFor TheMaskedHunter where
  getModifiersFor target (TheMaskedHunter a) | isTarget a target = do
    healthModifier <- getPlayerCountValue (PerPlayer 2)
    pure $ toModifiers a [HealthModifier healthModifier]
  getModifiersFor (InvestigatorTarget iid) (TheMaskedHunter a) = do
    affected <- iid <=~> investigatorEngagedWith (toId a)
    pure $ toModifiers a $ if affected
      then [CannotDiscoverClues, CannotSpendClues]
      else []
  getModifiersFor _ _ = pure []

instance RunMessage TheMaskedHunter where
  runMessage msg (TheMaskedHunter attrs) =
    TheMaskedHunter <$> runMessage msg attrs
