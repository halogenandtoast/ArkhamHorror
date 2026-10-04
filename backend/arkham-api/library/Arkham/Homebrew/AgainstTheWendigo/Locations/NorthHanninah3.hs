module Arkham.Homebrew.AgainstTheWendigo.Locations.NorthHanninah3 (northHanninah3) where

import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Helpers
import Arkham.Location.Import.Lifted

newtype NorthHanninah3 = NorthHanninah3 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

northHanninah3 :: LocationCard NorthHanninah3
northHanninah3 = location NorthHanninah3 Cards.northHanninah3 3 (PerPlayer 1)

instance HasAbilities NorthHanninah3 where
  getAbilities (NorthHanninah3 a) = extendRevealed a $ riverActions a

instance RunMessage NorthHanninah3 where
  runMessage msg l@(NorthHanninah3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      resolveWalkAlongTheRiver (attrs.ability 1) iid
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      resolveNavigate (attrs.ability 2) iid
      pure l
    _ -> NorthHanninah3 <$> liftRunMessage msg attrs
