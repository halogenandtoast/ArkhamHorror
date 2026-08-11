module Arkham.Homebrew.DarkMatter.Locations.QCrystalMines (qCrystalMines) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (placeFacedownInThreatArea)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype QCrystalMines = QCrystalMines LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

qCrystalMines :: LocationCard QCrystalMines
qCrystalMines = location QCrystalMines Cards.qCrystalMines 4 (PerPlayer 2)

{- | "Forced - After you enter Q-Crystal Mines, place the top card of the
encounter deck into your threat area, face-down."
-}
instance HasAbilities QCrystalMines where
  getAbilities (QCrystalMines a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ Enters #after You (be a)

instance RunMessage QCrystalMines where
  runMessage msg l@(QCrystalMines attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      placeFacedownInThreatArea iid 1
      pure l
    _ -> QCrystalMines <$> liftRunMessage msg attrs
