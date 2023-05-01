module Arkham.Location.Cards.HangmansBrook
  ( hangmansBrook
  , HangmansBrook(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype HangmansBrook = HangmansBrook LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hangmansBrook :: LocationCard HangmansBrook
hangmansBrook = location HangmansBrook Cards.hangmansBrook 4 (PerPlayer 1)

instance HasAbilities HangmansBrook where
  getAbilities (HangmansBrook attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage HangmansBrook where
  runMessage msg (HangmansBrook attrs) =
    HangmansBrook <$> runMessage msg attrs
