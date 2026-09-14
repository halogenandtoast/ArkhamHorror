module Arkham.Homebrew.DarkMatter.Locations.SpiralStaircase (spiralStaircase) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (flipToOtherSide)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype SpiralStaircase = SpiralStaircase LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spiralStaircase :: LocationCard SpiralStaircase
spiralStaircase =
  symbolLabel
    $ locationWith SpiralStaircase Cards.spiralStaircase 3 (PerPlayer 1) (canBeFlippedL .~ True)

instance HasAbilities SpiralStaircase where
  getAbilities (SpiralStaircase a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ FlipLocation #when Anyone (be a <> LocationWithInvestigator Anyone)

instance RunMessage SpiralStaircase where
  runMessage msg l@(SpiralStaircase attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      here <- select $ investigatorAt attrs.id
      for_ here \iid -> do
        connected <- select $ connectedTo (be attrs)
        chooseTargetM iid connected $ moveTo attrs iid
      pure l
    Flip flipper _ (isTarget attrs -> True) -> do
      flipToOtherSide flipper attrs
      pure l
    _ -> SpiralStaircase <$> liftRunMessage msg attrs
