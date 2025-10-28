module Arkham.Location.Cards.ReturnToHangmansBrookSpectral (returnToHangmansBrookSpectral) where

import Arkham.Ability
import Arkham.Card
import Arkham.GameValue
import Arkham.Helpers.Location
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.TheWagesOfSin.Helpers

newtype ReturnToHangmansBrookSpectral = ReturnToHangmansBrookSpectral LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToHangmansBrookSpectral :: LocationCard ReturnToHangmansBrookSpectral
returnToHangmansBrookSpectral = location ReturnToHangmansBrookSpectral Cards.returnToHangmansBrookSpectral 1 (Static 0)

instance HasAbilities ReturnToHangmansBrookSpectral where
  getAbilities (ReturnToHangmansBrookSpectral a) =
    extendRevealed
      a
      [ scenarioI18n $ withI18nTooltip "returnToHangmansBrookSpectral.resign" (locationResignAction a)
      , scenarioI18n $ hauntedI "returnToHangmansBrookSpectral.haunted" a 1
      ]

instance RunMessage ReturnToHangmansBrookSpectral where
  runMessage msg l@(ReturnToHangmansBrookSpectral attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      placeClues (attrs.ability 1) attrs 1
      pure l
    FlipThis (isTarget attrs -> True) -> do
      swapLocation attrs =<< genCard Locations.returnToHangmansBrook
      pure l
    _ -> ReturnToHangmansBrookSpectral <$> liftRunMessage msg attrs
