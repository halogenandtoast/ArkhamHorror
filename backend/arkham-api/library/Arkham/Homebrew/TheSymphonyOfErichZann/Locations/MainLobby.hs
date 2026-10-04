module Arkham.Homebrew.TheSymphonyOfErichZann.Locations.MainLobby (mainLobby) where

import Arkham.Ability
import Arkham.GameEnv (getPhase)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Phase

newtype MainLobby = MainLobby LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mainLobby :: LocationCard MainLobby
mainLobby = location MainLobby Cards.mainLobby 3 (PerPlayer 1)

instance HasModifiersFor MainLobby where
  -- "While you are in the Main Lobby, you cannot draw cards during the upkeep phase."
  getModifiersFor (MainLobby a) = do
    isUpkeep <- (== UpkeepPhase) <$> getPhase
    modifySelect a (investigatorAt a.id) [CannotDrawCards | isUpkeep]

instance HasAbilities MainLobby where
  -- "[action]: Draw 3 cards. (Group limit once per game)"
  getAbilities (MainLobby a) =
    extend1 a $ groupLimit PerGame $ restricted a 1 Here actionAbility

instance RunMessage MainLobby where
  runMessage msg l@(MainLobby attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCards iid (attrs.ability 1) 3
      pure l
    _ -> MainLobby <$> liftRunMessage msg attrs
