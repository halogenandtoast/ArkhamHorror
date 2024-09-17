module Arkham.Enemy.Cards.BarnabasMarshTheChangeIsUponHim
  ( barnabasMarshTheChangeIsUponHim
  , BarnabasMarshTheChangeIsUponHim(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype BarnabasMarshTheChangeIsUponHim = BarnabasMarshTheChangeIsUponHim EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

barnabasMarshTheChangeIsUponHim :: EnemyCard BarnabasMarshTheChangeIsUponHim
barnabasMarshTheChangeIsUponHim = enemy BarnabasMarshTheChangeIsUponHim Cards.barnabasMarshTheChangeIsUponHim (3, Static 4, 2) (1, 2)

instance RunMessage BarnabasMarshTheChangeIsUponHim where
  runMessage msg (BarnabasMarshTheChangeIsUponHim attrs) =
    BarnabasMarshTheChangeIsUponHim <$> runMessage msg attrs
