module Arkham.Homebrew.CircusExMortis.Locations.MountainStream_168 (mountainStream_168) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Helpers.Investigator (canHaveDamageHealed, canHaveHorrorHealed)
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype MountainStream_168 = MountainStream_168 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mountainStream_168 :: LocationCard MountainStream_168
mountainStream_168 = location MountainStream_168 Cards.mountainStream_168 3 (PerPlayer 1)

instance HasAbilities MountainStream_168 where
  getAbilities (MountainStream_168 a) =
    extendRevealed1 a
      $ restricted
        a
        1
        (Here <> oneOf [can.heal.damage (a.ability 1) You, can.heal.horror (a.ability 1) You])
        doubleActionAbility

instance RunMessage MountainStream_168 where
  runMessage msg l@(MountainStream_168 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      whenM (canHaveDamageHealed (attrs.ability 1) iid) $ healDamage iid (attrs.ability 1) 1
      whenM (canHaveHorrorHealed (attrs.ability 1) iid) $ healHorror iid (attrs.ability 1) 1
      pure l
    _ -> MountainStream_168 <$> liftRunMessage msg attrs
