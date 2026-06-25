module Arkham.Enemy.Cards.Oozeling (oozeling) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Oozified))

newtype Oozeling = Oozeling EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

oozeling :: EnemyCard Oozeling
oozeling =
  enemy Oozeling Cards.oozeling
    & setSpawnAt (EmptyLocation <> LocationWithTrait Oozified)
