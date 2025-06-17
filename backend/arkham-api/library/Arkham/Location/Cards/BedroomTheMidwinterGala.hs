module Arkham.Location.Cards.BedroomTheMidwinterGala (bedroomTheMidwinterGala) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype BedroomTheMidwinterGala = BedroomTheMidwinterGala LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | 'BedroomTheMidwinterGala' from The Midwinter Gala (#71012).
bedroomTheMidwinterGala :: LocationCard BedroomTheMidwinterGala
bedroomTheMidwinterGala = location BedroomTheMidwinterGala Cards.bedroomTheMidwinterGala 3 (PerPlayer 2)

instance HasAbilities BedroomTheMidwinterGala where
  getAbilities (BedroomTheMidwinterGala a) =
    extendRevealed1 a $ restricted a 1 Here actionAbility

instance RunMessage BedroomTheMidwinterGala where
  runMessage msg l@(BedroomTheMidwinterGala attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      -- TODO: Implement agility fight ability
      pure l
    _ -> BedroomTheMidwinterGala <$> liftRunMessage msg attrs
