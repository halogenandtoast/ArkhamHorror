module Arkham.Enemy.Cards.HotelManager
  ( hotelManager
  , HotelManager(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype HotelManager = HotelManager EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

hotelManager :: EnemyCard HotelManager
hotelManager = enemy HotelManager Cards.hotelManager (3, PerPlayer 6, 4) (2, 2)

instance RunMessage HotelManager where
  runMessage msg (HotelManager attrs) =
    HotelManager <$> runMessage msg attrs
