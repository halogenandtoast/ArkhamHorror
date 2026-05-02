module Arkham.Location.Cards.SewerTunnelsOvergrownTunnels (sewerTunnelsOvergrownTunnels) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (sewerTunnelsOvergrownTunnels)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype SewerTunnelsOvergrownTunnels = SewerTunnelsOvergrownTunnels LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sewerTunnelsOvergrownTunnels :: LocationCard SewerTunnelsOvergrownTunnels
sewerTunnelsOvergrownTunnels = location SewerTunnelsOvergrownTunnels Cards.sewerTunnelsOvergrownTunnels 2 (PerPlayer 1)

instance HasAbilities SewerTunnelsOvergrownTunnels where
  getAbilities (SewerTunnelsOvergrownTunnels a) =
    extendRevealed1 a $ restricted a 1 Here $ forced $ Enters #after You (be a)

instance RunMessage SewerTunnelsOvergrownTunnels where
  runMessage msg l@(SewerTunnelsOvergrownTunnels attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      loseActions iid (attrs.ability 1) 1
      pure l
    _ -> SewerTunnelsOvergrownTunnels <$> liftRunMessage msg attrs
