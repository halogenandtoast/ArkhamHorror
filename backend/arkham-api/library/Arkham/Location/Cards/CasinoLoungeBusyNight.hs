module Arkham.Location.Cards.CasinoLoungeBusyNight (casinoLoungeBusyNight) where

import Arkham.Ability
import Arkham.Helpers.Location (connectBothWays)
import Arkham.Helpers.Query (getJustLocationByName)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.Message.Lifted.Move
import Arkham.ScenarioLogKey

newtype CasinoLoungeBusyNight = CasinoLoungeBusyNight LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

casinoLoungeBusyNight :: LocationCard CasinoLoungeBusyNight
casinoLoungeBusyNight = symbolLabel $ location CasinoLoungeBusyNight Cards.casinoLoungeBusyNight 4 (PerPlayer 2)

instance HasAbilities CasinoLoungeBusyNight where
  getAbilities (CasinoLoungeBusyNight a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 (Here <> Remembered FoundAVent)
      $ ActionAbility [#move] (ActionCost 1 <> GroupClueCost (PerPlayer 2) (be a))

instance RunMessage CasinoLoungeBusyNight where
  runMessage msg l@(CasinoLoungeBusyNight attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      guardRoom <- getJustLocationByName "Guard Room"
      moveTo (attrs.ability 1) iid guardRoom
      selectEach (UnrevealedLocation <> "Staff Access Hallway") reveal
      remember TheVentIsOpen
      connectBothWays attrs guardRoom
      pure l
    _ -> CasinoLoungeBusyNight <$> liftRunMessage msg attrs
