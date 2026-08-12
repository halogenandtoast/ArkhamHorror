module Arkham.Homebrew.DarkMatter.Locations.InfirmaryTatterdemalion (infirmaryTatterdemalion) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (scan, scanAction_)
import Arkham.Location.Import.Lifted
import Arkham.LocationSymbol qualified as LS

newtype InfirmaryTatterdemalion = InfirmaryTatterdemalion LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

infirmaryTatterdemalion :: LocationCard InfirmaryTatterdemalion
infirmaryTatterdemalion =
  symbolLabel $ location InfirmaryTatterdemalion Cards.infirmaryTatterdemalion 3 (PerPlayer 1)

instance HasAbilities InfirmaryTatterdemalion where
  getAbilities (InfirmaryTatterdemalion a) =
    extendRevealed1 a $ restricted a 1 Here scanAction_

instance RunMessage InfirmaryTatterdemalion where
  runMessage msg l@(InfirmaryTatterdemalion attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      scan iid (attrs.ability 1) [LS.Heart]
      pure l
    _ -> InfirmaryTatterdemalion <$> liftRunMessage msg attrs
