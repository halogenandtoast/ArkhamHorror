module Arkham.Enemy.Cards.BrethrenOfAsh.FlyingTerrors.BatHorror (batHorror) where

import Arkham.Enemy.CardDefs.BrethrenOfAsh.FlyingTerrors qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype BatHorror = BatHorror EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

batHorror :: EnemyCard BatHorror
batHorror = enemy BatHorror Cards.batHorror
