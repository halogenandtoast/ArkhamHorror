module Arkham.Location.Cards.HangmansBrookSpectral
  ( hangmansBrookSpectral
  , HangmansBrookSpectral(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype HangmansBrookSpectral = HangmansBrookSpectral LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hangmansBrookSpectral :: LocationCard HangmansBrookSpectral
hangmansBrookSpectral = location HangmansBrookSpectral Cards.hangmansBrookSpectral 1 (Static 0)

instance HasAbilities HangmansBrookSpectral where
  getAbilities (HangmansBrookSpectral attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage HangmansBrookSpectral where
  runMessage msg (HangmansBrookSpectral attrs) =
    HangmansBrookSpectral <$> runMessage msg attrs
