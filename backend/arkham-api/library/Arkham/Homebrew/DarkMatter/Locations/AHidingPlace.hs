module Arkham.Homebrew.DarkMatter.Locations.AHidingPlace (aHidingPlace) where

import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhenM)
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype AHidingPlace = AHidingPlace LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

aHidingPlace :: LocationCard AHidingPlace
aHidingPlace = symbolLabel $ location AHidingPlace Cards.aHidingPlace 4 (PerPlayer 1)

{- | "While the investigator controlling K2-PS187 is at this location, reduce its
shroud by 2."
-}
instance HasModifiersFor AHidingPlace where
  getModifiersFor (AHidingPlace a) =
    modifySelfWhenM
      a
      (selectAny $ InvestigatorAt (be a) <> HasMatchingAsset (AssetWithTitle "K2-PS187"))
      [ShroudModifier (-2)]

instance RunMessage AHidingPlace where
  runMessage msg (AHidingPlace attrs) = AHidingPlace <$> runMessage msg attrs
