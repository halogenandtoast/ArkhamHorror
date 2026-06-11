module Arkham.Location.Cards.ChamberOfDecay (chamberOfDecay) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ChamberOfDecay = ChamberOfDecay LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chamberOfDecay :: LocationCard ChamberOfDecay
chamberOfDecay = location ChamberOfDecay Cards.chamberOfDecay 2 (PerPlayer 1)

instance HasAbilities ChamberOfDecay where
  getAbilities (ChamberOfDecay a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> thisExists a LocationWithoutClues <> exists (SetAsideCardMatch $ cardIs Assets.decayDiagram))
      $ FastAbility Free

instance RunMessage ChamberOfDecay where
  runMessage msg l@(ChamberOfDecay attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      card <- getSetAsideCard Assets.decayDiagram
      takeControlOfSetAsideAsset iid card
      pure l
    _ -> ChamberOfDecay <$> liftRunMessage msg attrs
