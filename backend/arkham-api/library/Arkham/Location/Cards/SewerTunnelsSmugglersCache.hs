module Arkham.Location.Cards.SewerTunnelsSmugglersCache (sewerTunnelsSmugglersCache) where

import Arkham.Ability
import Arkham.Capability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (sewerTunnelsSmugglersCache)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype SewerTunnelsSmugglersCache = SewerTunnelsSmugglersCache LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sewerTunnelsSmugglersCache :: LocationCard SewerTunnelsSmugglersCache
sewerTunnelsSmugglersCache = location SewerTunnelsSmugglersCache Cards.sewerTunnelsSmugglersCache 3 (Static 0)

instance HasAbilities SewerTunnelsSmugglersCache where
  getAbilities (SewerTunnelsSmugglersCache a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> can.gain.resources You)
      $ freeReaction
      $ TurnEnds #when You

instance RunMessage SewerTunnelsSmugglersCache where
  runMessage msg l@(SewerTunnelsSmugglersCache attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gainResources iid (attrs.ability 1) 1
      pure l
    _ -> SewerTunnelsSmugglersCache <$> liftRunMessage msg attrs
