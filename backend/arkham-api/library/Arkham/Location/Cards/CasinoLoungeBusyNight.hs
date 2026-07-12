module Arkham.Location.Cards.CasinoLoungeBusyNight (casinoLoungeBusyNight) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Helpers.Query (getJustLocationByName)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.Message.Lifted.Move
import Arkham.ScenarioLogKey

newtype CasinoLoungeBusyNight = CasinoLoungeBusyNight LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

casinoLoungeBusyNight :: LocationCard CasinoLoungeBusyNight
casinoLoungeBusyNight = symbolLabel $ location CasinoLoungeBusyNight Cards.casinoLoungeBusyNight 4 (PerPlayer 2)

-- The vent lets *investigators* move as if Casino Lounge and Guard Room were
-- connected; it is not a physical connection, so enemy (hunter) movement must
-- not traverse it. ForMovementConnectedToWhen is only honored for movement
-- queries (ForMovement), which the hunter path (NotForMovement) ignores.
instance HasModifiersFor CasinoLoungeBusyNight where
  getModifiersFor (CasinoLoungeBusyNight a) = do
    ventOpen <- remembered TheVentIsOpen
    when ventOpen do
      modifySelf a [ForMovementConnectedToWhen (be a) "Guard Room"]
      modifySelect a (location_ "Guard Room") [ForMovementConnectedToWhen "Guard Room" (be a)]

instance HasAbilities CasinoLoungeBusyNight where
  getAbilities (CasinoLoungeBusyNight a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 (Here <> Remembered FoundAVent)
      $ ActionAbility #move Nothing (ActionCost 1 <> GroupClueCost (PerPlayer 2) (be a))

instance RunMessage CasinoLoungeBusyNight where
  runMessage msg l@(CasinoLoungeBusyNight attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      guardRoom <- getJustLocationByName "Guard Room"
      moveTo (attrs.ability 1) iid guardRoom
      selectEach (UnrevealedLocation <> "Staff Access Hallway") reveal
      remember TheVentIsOpen
      pure l
    _ -> CasinoLoungeBusyNight <$> liftRunMessage msg attrs
