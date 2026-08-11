module Arkham.Homebrew.DarkMatter.Locations.IceCavity (iceCavity) where

import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (flipToOtherSide)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Cave))

newtype IceCavity = IceCavity LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

iceCavity :: LocationCard IceCavity
iceCavity = locationWith IceCavity Cards.iceCavity 6 (PerPlayer 1) (canBeFlippedL .~ True)

-- | "Ice Cavity gets -1 shroud for each connecting [[Cave]] location."
instance HasModifiersFor IceCavity where
  getModifiersFor (IceCavity a) = do
    caves <- selectCount $ connectedTo (be a) <> LocationWithTrait Cave
    modifySelf a [ShroudModifier (negate caves)]

instance RunMessage IceCavity where
  runMessage msg l@(IceCavity attrs) = runQueueT $ case msg of
    Flip _ _ (isTarget attrs -> True) -> do
      flipToOtherSide attrs
      pure l
    _ -> IceCavity <$> liftRunMessage msg attrs
