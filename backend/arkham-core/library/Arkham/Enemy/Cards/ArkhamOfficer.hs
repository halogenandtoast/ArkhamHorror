module Arkham.Enemy.Cards.ArkhamOfficer
  ( arkhamOfficer
  , ArkhamOfficer(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype ArkhamOfficer = ArkhamOfficer EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

arkhamOfficer :: EnemyCard ArkhamOfficer
arkhamOfficer = enemy ArkhamOfficer Cards.arkhamOfficer (3, Static 3, 2) (1, 0)

instance RunMessage ArkhamOfficer where
  runMessage msg (ArkhamOfficer attrs) =
    ArkhamOfficer <$> runMessage msg attrs
