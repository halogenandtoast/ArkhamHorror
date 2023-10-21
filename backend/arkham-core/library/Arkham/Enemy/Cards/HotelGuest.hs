module Arkham.Enemy.Cards.HotelGuest
  ( hotelGuest
  , HotelGuest(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype HotelGuest = HotelGuest EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

hotelGuest :: EnemyCard HotelGuest
hotelGuest = enemy HotelGuest Cards.hotelGuest (1, Static 1, 2) (1, 0)

instance RunMessage HotelGuest where
  runMessage msg (HotelGuest attrs) =
    HotelGuest <$> runMessage msg attrs
