module Arkham.Event.Events.Shortcut2 (shortcut2) where

import Arkham.Ability
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Location (getAccessibleLocations)
import Arkham.Matcher
import Arkham.Message.Lifted.Move

newtype Shortcut2 = Shortcut2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shortcut2 :: EventCard Shortcut2
shortcut2 = event Shortcut2 Cards.shortcut2

instance HasAbilities Shortcut2 where
  getAbilities (Shortcut2 a) = case a.attachedTo.location of
    Just lid ->
      [ restricted
          (proxied lid a)
          1
          (OnLocation (LocationWithId lid) <> LocationExists AccessibleLocation)
          (FastAbility' (exhaust a) [#move])
      ]
    _ -> []

instance RunMessage Shortcut2 where
  runMessage msg e@(Shortcut2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      lid <- selectJust $ locationWithInvestigator iid
      place attrs (AttachedToLocation lid)
      pure e
    UseThisAbility iid (ProxySource _ (isSource attrs -> True)) 1 -> do
      connectingLocations <- getAccessibleLocations iid attrs
      chooseTargetM iid connectingLocations (moveTo (toSource attrs) iid)
      pure e
    _ -> Shortcut2 <$> liftRunMessage msg attrs
