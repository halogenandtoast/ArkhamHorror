module Arkham.Location.Cards.CityOfTheUnseen (cityOfTheUnseen) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Id
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Window

newtype Metadata = Metadata {inUse :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype CityOfTheUnseen = CityOfTheUnseen (LocationAttrs `With` Metadata)
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cityOfTheUnseen :: LocationCard CityOfTheUnseen
cityOfTheUnseen =
  location
    (CityOfTheUnseen . (`with` Metadata False))
    Cards.cityOfTheUnseen
    4
    (Static 1)

instance HasAbilities CityOfTheUnseen where
  getAbilities (CityOfTheUnseen (attrs `With` meta)) =
    withBaseAbilities
      attrs
      [ groupLimit PerWindow
          $ restricted attrs 1 (if inUse meta then Never else NoRestriction)
          $ forced
          $ PlacedCounterOnEnemy #after (enemyAt $ toId attrs) AnySource DoomCounter (atLeast 1)
      ]

getEnemyId :: [Window] -> EnemyId
getEnemyId [] = error "invalid window"
getEnemyId ((windowType -> PlacedDoom _ (EnemyTarget eid) _) : _) = eid
getEnemyId (_ : xs) = getEnemyId xs

instance RunMessage CityOfTheUnseen where
  runMessage msg (CityOfTheUnseen (attrs `With` meta)) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getEnemyId -> enemyId) payment ->
      do
        pushAll
          [ PlaceDoom (toAbilitySource attrs 1) (toTarget enemyId) 1
          , UseCardAbilityChoice iid (toSource attrs) 1 NoAbilityMetadata [] payment
          ]
        pure . CityOfTheUnseen $ attrs `with` Metadata True
    UseCardAbilityChoice _ (isSource attrs -> True) _ _ _ _ -> do
      pure . CityOfTheUnseen $ attrs `with` Metadata False
    _ -> CityOfTheUnseen . (`with` meta) <$> runMessage msg attrs
