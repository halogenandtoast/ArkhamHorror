module Arkham.Location.Cards.CanalsOfTenochtitlan_180 (
  canalsOfTenochtitlan_180,
  CanalsOfTenochtitlan_180 (..),
) where

import Arkham.Prelude

import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher

newtype CanalsOfTenochtitlan_180 = CanalsOfTenochtitlan_180 LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

canalsOfTenochtitlan_180 :: LocationCard CanalsOfTenochtitlan_180
canalsOfTenochtitlan_180 =
  locationWith
    CanalsOfTenochtitlan_180
    Cards.canalsOfTenochtitlan_180
    5
    (PerPlayer 1)
    (labelL .~ "diamond")

instance HasModifiersFor CanalsOfTenochtitlan_180 where
  getModifiersFor (EnemyTarget eid) (CanalsOfTenochtitlan_180 a) = do
    here <- eid <=~> enemyAt (toId a)
    pure $ toModifiers a [EnemyEvade 2 | here]
  getModifiersFor target (CanalsOfTenochtitlan_180 a) | isTarget a target = do
    exhaustedEnemy <- selectAny $ ExhaustedEnemy <> enemyAt (toId a)
    pure $ toModifiers a [ShroudModifier (-3) | exhaustedEnemy]
  getModifiersFor _ _ = pure []

instance RunMessage CanalsOfTenochtitlan_180 where
  runMessage msg (CanalsOfTenochtitlan_180 attrs) =
    CanalsOfTenochtitlan_180 <$> runMessage msg attrs
