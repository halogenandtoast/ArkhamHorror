module Arkham.Homebrew.AgesUnwound.Locations.Stables (stables) where

import Arkham.GameValue
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype Stables = Stables LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

{- | "The Stables are investigated using [combat] instead of the skill indicated
by the investigation attempt."
-}
stables :: LocationCard Stables
stables =
  symbolLabel
    $ locationWith Stables Cards.stables 3 (PerPlayer 1) (investigateSkillL .~ #combat)

instance RunMessage Stables where
  runMessage msg (Stables attrs) = Stables <$> runMessage msg attrs
