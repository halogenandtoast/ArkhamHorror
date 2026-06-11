module Arkham.Location.Cards.ChamberOfHunger (chamberOfHunger) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ChamberOfHunger = ChamberOfHunger LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chamberOfHunger :: LocationCard ChamberOfHunger
chamberOfHunger = location ChamberOfHunger Cards.chamberOfHunger 3 (PerPlayer 1)

instance HasAbilities ChamberOfHunger where
  getAbilities (ChamberOfHunger a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> thisExists a LocationWithoutClues <> exists (SetAsideCardMatch $ cardIs Assets.hungerDiagram))
      $ FastAbility Free

instance RunMessage ChamberOfHunger where
  runMessage msg l@(ChamberOfHunger attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      card <- getSetAsideCard Assets.hungerDiagram
      takeControlOfSetAsideAsset iid card
      pure l
    _ -> ChamberOfHunger <$> liftRunMessage msg attrs
