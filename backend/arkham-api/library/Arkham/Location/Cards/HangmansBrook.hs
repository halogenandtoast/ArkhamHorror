module Arkham.Location.Cards.HangmansBrook (hangmansBrook) where

import Arkham.Ability
import Arkham.Card
import Arkham.GameValue
import Arkham.Helpers.Location
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.TheWagesOfSin.Helpers

newtype HangmansBrook = HangmansBrook LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hangmansBrook :: LocationCard HangmansBrook
hangmansBrook = location HangmansBrook Cards.hangmansBrook 4 (PerPlayer 1)

instance HasAbilities HangmansBrook where
  getAbilities (HangmansBrook a) =
    extendRevealed1 a $ scenarioI18n $ withI18nTooltip "hangmansBrook.resign" (locationResignAction a)

instance RunMessage HangmansBrook where
  runMessage msg l@(HangmansBrook attrs) = runQueueT $ case msg of
    FlipThis (isTarget attrs -> True) -> do
      swapLocation attrs =<< genCard Locations.hangmansBrookSpectral
      pure l
    _ -> HangmansBrook <$> liftRunMessage msg attrs
