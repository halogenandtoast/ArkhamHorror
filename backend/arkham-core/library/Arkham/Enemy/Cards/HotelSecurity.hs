module Arkham.Enemy.Cards.HotelSecurity (
  hotelSecurity,
  HotelSecurity (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Trait (Trait (Guest))

newtype HotelSecurity = HotelSecurity EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData, HasAbilities)

hotelSecurity :: EnemyCard HotelSecurity
hotelSecurity =
  enemyWith
    HotelSecurity
    Cards.hotelSecurity
    (4, Static 3, 2)
    (2, 0)
    (spawnAtL ?~ SpawnAt (NearestLocationToYou EmptyLocation))

instance HasModifiersFor HotelSecurity where
  getModifiersFor target (HotelSecurity attrs) | attrs `is` target = do
    anyGuests <- selectAny $ EnemyWithTrait Guest
    pure
      $ toModifiers
        attrs
      $ guard anyGuests
      *> [ RemoveKeyword Keyword.Hunter
         , AddKeyword (Keyword.Patrol $ LocationWithEnemy $ EnemyWithTrait Guest)
         ]
  getModifiersFor _ _ = pure []

instance RunMessage HotelSecurity where
  runMessage msg (HotelSecurity attrs) =
    HotelSecurity <$> runMessage msg attrs
