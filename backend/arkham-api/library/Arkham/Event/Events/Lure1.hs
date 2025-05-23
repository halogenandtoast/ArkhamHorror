module Arkham.Event.Events.Lure1 (lure1) where

import Arkham.Ability
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Location
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype Lure1 = Lure1 EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lure1 :: EventCard Lure1
lure1 = event Lure1 Cards.lure1

instance HasAbilities Lure1 where
  getAbilities (Lure1 attrs) = [restricted attrs 1 ControlsThis $ forced $ RoundEnds #when]

instance HasModifiersFor Lure1 where
  getModifiersFor (Lure1 attrs) =
    for_ attrs.attachedTo.location \lid ->
      modifySelect attrs AnyEnemy [DuringEnemyPhaseMustMoveToward (toTarget lid)]

instance RunMessage Lure1 where
  runMessage msg e@(Lure1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      withLocationOf iid $ place attrs . AttachedToLocation
      pure e
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure e
    _ -> Lure1 <$> liftRunMessage msg attrs
