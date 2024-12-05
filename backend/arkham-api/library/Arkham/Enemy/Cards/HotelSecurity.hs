module Arkham.Enemy.Cards.HotelSecurity (hotelSecurity, HotelSecurity (..)) where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Trait (Trait (Guest))

newtype HotelSecurity = HotelSecurity EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

hotelSecurity :: EnemyCard HotelSecurity
hotelSecurity =
  enemyWith HotelSecurity Cards.hotelSecurity (4, Static 3, 2) (2, 0)
    $ spawnAtL
    ?~ SpawnAt (NearestLocationToYou EmptyLocation)

instance HasModifiersFor HotelSecurity where
  getModifiersFor (HotelSecurity attrs) = do
    anyGuests <- selectAny $ EnemyWithTrait Guest
    modifySelfWhen
      attrs
      anyGuests
      [ RemoveKeyword Keyword.Hunter
      , AddKeyword (Keyword.Patrol $ LocationWithEnemy $ EnemyWithTrait Guest)
      ]

instance RunMessage HotelSecurity where
  runMessage msg (HotelSecurity attrs) = HotelSecurity <$> runMessage msg attrs
