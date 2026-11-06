module Arkham.Homebrew.NightOfVespers.Locations.LuxurySuites_021 (luxurySuites_021) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Spawn

newtype LuxurySuites_021 = LuxurySuites_021 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

luxurySuites_021 :: LocationCard LuxurySuites_021
luxurySuites_021 = symbolLabel $ location LuxurySuites_021 Cards.luxurySuites_021 2 (PerPlayer 2)

-- The redirect is live only for the card this reveal turns up, so it is held in
-- meta across the draw rather than applied for as long as the location is out.
instance HasModifiersFor LuxurySuites_021 where
  getModifiersFor (LuxurySuites_021 a) =
    when (getLocationMetaDefault False a)
      $ modifySelect a AnyEnemy [OverwrittenSpawn (SpawnAt (be a))]

-- Printed on the unrevealed side but offered on both, because the window resolves
-- after this location is revealed. It rides the after-move window so that the
-- act's after-enter removal of unrevealed locations resolves first.
instance HasAbilities LuxurySuites_021 where
  getAbilities (LuxurySuites_021 a) =
    extend a
      $ mkAbility a 1 (forced $ Moves #after You AnySource Anywhere (be a))
      : (guard a.revealed *> [mkAbility a 2 $ forced $ RevealLocation #when You (be a)])

instance RunMessage LuxurySuites_021 where
  runMessage msg l@(LuxurySuites_021 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeSetAsideLocationsMatching_
        $ mapOneOf
          cardIs
          [ Cards.conservatory
          , Cards.hotelRooftop
          ]
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      drawEncounterCard iid (attrs.ability 2)
      doStep 1 msg
      pure . LuxurySuites_021 $ attrs & setMeta True
    DoStep 1 (UseThisAbility _ (isSource attrs -> True) 2) -> do
      pure . LuxurySuites_021 $ attrs & setMeta False
    _ -> LuxurySuites_021 <$> liftRunMessage msg attrs
