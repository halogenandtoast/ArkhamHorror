module Arkham.Homebrew.DarkMatter.Locations.EntranceTunnel (entranceTunnel) where

import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype EntranceTunnel = EntranceTunnel LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

entranceTunnel :: LocationCard EntranceTunnel
entranceTunnel = location EntranceTunnel Cards.entranceTunnel 3 (Static 0)

-- "[action]: Resign. Return to the Tatterdemalion with all of your findings."
instance HasAbilities EntranceTunnel where
  getAbilities (EntranceTunnel a) = extendRevealed1 a $ locationResignAction a

instance RunMessage EntranceTunnel where
  runMessage msg (EntranceTunnel attrs) = EntranceTunnel <$> runMessage msg attrs
