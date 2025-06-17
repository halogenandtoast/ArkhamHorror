module Arkham.Location.Cards.LanternChamber (lanternChamber) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype LanternChamber = LanternChamber LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | 'Lantern Chamber' from The Midwinter Gala (#71008).
lanternChamber :: LocationCard LanternChamber
lanternChamber =
  locationWith LanternChamber Cards.lanternChamber 5 (PerPlayer 2)
    $ costToEnterUnrevealedL
    .~ HorrorCost ThisCard YouTarget 1

instance HasAbilities LanternChamber where
  getAbilities (LanternChamber a) =
    extendRevealed
      a
      [ groupLimit PerTurn $ restricted a 1 Here $ FastAbility Free
      , restricted a 2 Here actionAbility
      ]

instance RunMessage LanternChamber where
  runMessage msg l@(LanternChamber attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      -- TODO: Implement guessing ability
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      -- TODO: Implement spellbound flipping ability
      pure l
    _ -> LanternChamber <$> liftRunMessage msg attrs
