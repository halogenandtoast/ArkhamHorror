module Arkham.Enemy.Cards.HotelSecurity
  ( hotelSecurity
  , HotelSecurity(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype HotelSecurity = HotelSecurity EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

hotelSecurity :: EnemyCard HotelSecurity
hotelSecurity = enemy HotelSecurity Cards.hotelSecurity (4, Static 3, 2) (2, 0)

instance RunMessage HotelSecurity where
  runMessage msg (HotelSecurity attrs) =
    HotelSecurity <$> runMessage msg attrs
