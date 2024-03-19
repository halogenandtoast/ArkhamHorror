module Arkham.Enemy.Cards.NyarlathotepTheFacelessWhisperer
  ( nyarlathotepTheFacelessWhisperer
  , NyarlathotepTheFacelessWhisperer(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype NyarlathotepTheFacelessWhisperer = NyarlathotepTheFacelessWhisperer EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

nyarlathotepTheFacelessWhisperer :: EnemyCard NyarlathotepTheFacelessWhisperer
nyarlathotepTheFacelessWhisperer = enemy NyarlathotepTheFacelessWhisperer Cards.nyarlathotepTheFacelessWhisperer (4, Static 3, 3) (1, 0)

instance RunMessage NyarlathotepTheFacelessWhisperer where
  runMessage msg (NyarlathotepTheFacelessWhisperer attrs) =
    NyarlathotepTheFacelessWhisperer <$> runMessage msg attrs
