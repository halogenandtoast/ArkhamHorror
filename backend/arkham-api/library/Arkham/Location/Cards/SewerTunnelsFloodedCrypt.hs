module Arkham.Location.Cards.SewerTunnelsFloodedCrypt (sewerTunnelsFloodedCrypt) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (sewerTunnelsFloodedCrypt)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype SewerTunnelsFloodedCrypt = SewerTunnelsFloodedCrypt LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sewerTunnelsFloodedCrypt :: LocationCard SewerTunnelsFloodedCrypt
sewerTunnelsFloodedCrypt = location SewerTunnelsFloodedCrypt Cards.sewerTunnelsFloodedCrypt 4 (PerPlayer 1)

instance HasAbilities SewerTunnelsFloodedCrypt where
  getAbilities (SewerTunnelsFloodedCrypt a) =
    extendRevealed1 a $ restricted a 1 Here $ forced $ TurnEnds #when You

instance RunMessage SewerTunnelsFloodedCrypt where
  runMessage msg l@(SewerTunnelsFloodedCrypt attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 1
      pure l
    _ -> SewerTunnelsFloodedCrypt <$> liftRunMessage msg attrs
