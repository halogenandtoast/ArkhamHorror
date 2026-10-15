module Arkham.Homebrew.TheFallOfTheHouseOfUsher.Locations.RodericksStudio (rodericksStudio) where

import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.Helpers
import Arkham.Location.Import.Lifted

newtype RodericksStudio = RodericksStudio LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rodericksStudio :: LocationCard RodericksStudio
rodericksStudio = location RodericksStudio Cards.rodericksStudio 4 (Static 0)

instance HasModifiersFor RodericksStudio where
  getModifiersFor (RodericksStudio a) = whenRevealed a $ hereGets a [ControlledAssetsCannotReady]

instance HasAbilities RodericksStudio where
  getAbilities (RodericksStudio a) =
    extendRevealed1 a
      $ scenarioI18n
      $ collapseForced "rodericksStudio.collapse" a 1

instance RunMessage RodericksStudio where
  runMessage msg l@(RodericksStudio attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      flipToRuined attrs
      pure l
    _ -> RodericksStudio <$> liftRunMessage msg attrs
