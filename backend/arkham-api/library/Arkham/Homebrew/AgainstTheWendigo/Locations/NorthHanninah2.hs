module Arkham.Homebrew.AgainstTheWendigo.Locations.NorthHanninah2 (northHanninah2) where

import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Helpers
import Arkham.Location.Import.Lifted

newtype NorthHanninah2 = NorthHanninah2 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

northHanninah2 :: LocationCard NorthHanninah2
northHanninah2 = location NorthHanninah2 Cards.northHanninah2 3 (PerPlayer 1)

instance HasAbilities NorthHanninah2 where
  getAbilities (NorthHanninah2 a) = extendRevealed a $ riverActions a

instance RunMessage NorthHanninah2 where
  runMessage msg l@(NorthHanninah2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      resolveWalkAlongTheRiver (attrs.ability 1) iid
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      resolveNavigate (attrs.ability 2) iid
      pure l
    _ -> NorthHanninah2 <$> liftRunMessage msg attrs
