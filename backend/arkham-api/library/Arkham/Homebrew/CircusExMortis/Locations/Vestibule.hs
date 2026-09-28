module Arkham.Homebrew.CircusExMortis.Locations.Vestibule (vestibule) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (scenarioI18n)
import Arkham.Location.Import.Lifted

newtype Vestibule = Vestibule LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vestibule :: LocationCard Vestibule
vestibule = location Vestibule Cards.vestibule 1 (Static 0)

instance HasAbilities Vestibule where
  getAbilities (Vestibule a) =
    extendRevealed1 a
      $ scenarioI18n "bacchanalia"
      $ withI18nTooltip "vestibule.resign"
      $ locationResignAction a
