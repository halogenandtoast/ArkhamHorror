module Arkham.Location.Cards.CityOfTheUnseen (cityOfTheUnseen) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Id
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Window

newtype Metadata = Metadata {inUse :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype CityOfTheUnseen = CityOfTheUnseen (LocationAttrs `With` Metadata)
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cityOfTheUnseen :: LocationCard CityOfTheUnseen
cityOfTheUnseen = location (CityOfTheUnseen . (`with` Metadata False)) Cards.cityOfTheUnseen 4 (Static 1)

instance HasAbilities CityOfTheUnseen where
  getAbilities (CityOfTheUnseen (a `With` meta)) =
    extendRevealed1 a
      $ groupLimit PerWindow
      $ restricted a 1 (if inUse meta then Never else NoRestriction)
      $ forced
      $ PlacedCounterOnEnemy #after (enemyAt $ toId a) AnySource DoomCounter (atLeast 1)

getEnemyId :: [Window] -> EnemyId
getEnemyId [] = error "invalid window"
getEnemyId ((windowType -> PlacedDoom _ (EnemyTarget eid) _) : _) = eid
getEnemyId (_ : xs) = getEnemyId xs

instance RunMessage CityOfTheUnseen where
  runMessage msg (CityOfTheUnseen (attrs `With` meta)) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getEnemyId -> enemyId) _ -> do
      placeDoom (attrs.ability 1) enemyId 1
      doStep 1 msg
      pure . CityOfTheUnseen $ attrs `with` Metadata True
    DoStep 1 (UseThisAbility _ (isSource attrs -> True) 1) -> do
      pure . CityOfTheUnseen $ attrs `with` Metadata False
    _ -> CityOfTheUnseen . (`with` meta) <$> liftRunMessage msg attrs
