module Arkham.Enemy.Cards.BrethrenOfAsh.AshenPilgrims.CantorOfFlame (cantorOfFlame) where

import Arkham.Enemy.CardDefs.BrethrenOfAsh.AshenPilgrims qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype CantorOfFlame = CantorOfFlame EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

cantorOfFlame :: EnemyCard CantorOfFlame
cantorOfFlame = enemy CantorOfFlame Cards.cantorOfFlame
