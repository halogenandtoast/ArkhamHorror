module Arkham.Enemy.Cards.BrethrenOfAsh.SpreadingFlames.Bystander (bystander) where

import Arkham.Enemy.CardDefs.BrethrenOfAsh.SpreadingFlames qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype Bystander = Bystander EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

bystander :: EnemyCard Bystander
bystander = enemy Bystander Cards.bystander
