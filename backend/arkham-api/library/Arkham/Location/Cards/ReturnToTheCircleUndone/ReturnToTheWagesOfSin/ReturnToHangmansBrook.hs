module Arkham.Location.Cards.ReturnToTheCircleUndone.ReturnToTheWagesOfSin.ReturnToHangmansBrook (returnToHangmansBrook) where

import Arkham.Ability
import Arkham.Card
import Arkham.GameValue
import Arkham.Helpers.Location
import Arkham.Location.CardDefs.ReturnToTheCircleUndone.ReturnToTheWagesOfSin qualified as Cards
import Arkham.Location.CardDefs.ReturnToTheCircleUndone.ReturnToTheWagesOfSin qualified as Locations
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.TheCircleUndone.TheWagesOfSin.Helpers

newtype ReturnToHangmansBrook = ReturnToHangmansBrook LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToHangmansBrook :: LocationCard ReturnToHangmansBrook
returnToHangmansBrook = location ReturnToHangmansBrook Cards.returnToHangmansBrook 4 (PerPlayer 1)

instance HasAbilities ReturnToHangmansBrook where
  getAbilities (ReturnToHangmansBrook a) =
    extendRevealed1 a $ scenarioI18n $ withI18nTooltip "hangmansBrook.resign" (locationResignAction a)

instance RunMessage ReturnToHangmansBrook where
  runMessage msg l@(ReturnToHangmansBrook attrs) = runQueueT $ case msg of
    FlipThis (isTarget attrs -> True) -> do
      swapLocation attrs =<< genCard Locations.returnToHangmansBrookSpectral
      pure l
    _ -> ReturnToHangmansBrook <$> liftRunMessage msg attrs
