{- | The runner behind a debug-authored custom location. See "Arkham.Custom.Enemy".

Shroud and clue value are not 'CardDef' fields, so they come from the def's meta
(@shroud@/@revealClues@), which is where the debug editor puts them.
-}
module Arkham.Custom.Location (CustomLocation (..), customLocation) where

import Arkham.Card.CardDef (CardDef)
import Arkham.Card.CustomCard (customMeta)
import Arkham.GameValue
import Arkham.Location.Import.Lifted

newtype CustomLocation = CustomLocation LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

customLocation :: CardDef -> LocationCard CustomLocation
customLocation def =
  location CustomLocation def (customMeta "shroud" 0 def) (customMeta "revealClues" (Static 0) def)

instance RunMessage CustomLocation where
  runMessage msg (CustomLocation attrs) = CustomLocation <$> runMessage msg attrs
