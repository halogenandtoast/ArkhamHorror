module Arkham.Location.Cards.ChamberOfRot (chamberOfRot) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ChamberOfRot = ChamberOfRot LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chamberOfRot :: LocationCard ChamberOfRot
chamberOfRot = location ChamberOfRot Cards.chamberOfRot 3 (PerPlayer 1)

instance HasAbilities ChamberOfRot where
  getAbilities (ChamberOfRot a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> thisExists a LocationWithoutClues <> exists (SetAsideCardMatch $ cardIs Assets.rotDiagram))
      $ FastAbility Free

instance RunMessage ChamberOfRot where
  runMessage msg l@(ChamberOfRot attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      card <- getSetAsideCard Assets.rotDiagram
      takeControlOfSetAsideAsset iid card
      pure l
    _ -> ChamberOfRot <$> liftRunMessage msg attrs
