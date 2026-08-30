module Arkham.Location.Cards.TheDrownedCity.TheWesternWall.WesternWall_11530 (westernWall_11530) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (BaseShroud), modifySelf)
import Arkham.Location.CardDefs.TheDrownedCity.TheWesternWall qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.TheDrownedCity.TheWesternWall.Helpers (
  cannotEnterFromCluedLocation,
  locationLevel,
 )

newtype WesternWall_11530 = WesternWall_11530 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

westernWall_11530 :: LocationCard WesternWall_11530
westernWall_11530 = withXShroud $ location WesternWall_11530 Cards.westernWall_11530 0 (Static 2)

instance HasModifiersFor WesternWall_11530 where
  getModifiersFor (WesternWall_11530 a) = do
    modifySelf a [BaseShroud $ maybe 1 locationLevel $ locationPosition a]
    cannotEnterFromCluedLocation a

instance HasAbilities WesternWall_11530 where
  getAbilities (WesternWall_11530 a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 Here doubleActionAbility

instance RunMessage WesternWall_11530 where
  runMessage msg l@(WesternWall_11530 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCards iid (attrs.ability 1) 3
      pure l
    _ -> WesternWall_11530 <$> liftRunMessage msg attrs
