-- | The runner behind a debug-authored custom event. See "Arkham.Custom.Enemy".
module Arkham.Custom.Event (CustomEvent (..), customEvent) where

import Arkham.Card.CardDef (CardDef)
import Arkham.Custom.Ability (
  customAbilities,
  customModifiers,
  isCustomAbility,
  runCustomAbility,
  runCustomHandlers,
  runCustomSteps,
 )
import Arkham.Event.Import.Lifted

newtype CustomEvent = CustomEvent EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

customEvent :: CardDef -> EventCard CustomEvent
customEvent = event CustomEvent

instance HasModifiersFor CustomEvent where
  getModifiersFor (CustomEvent a) = customModifiers a

instance HasAbilities CustomEvent where
  getAbilities (CustomEvent a) = customAbilities a

instance RunMessage CustomEvent where
  runMessage msg x@(CustomEvent attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) idx | isCustomAbility attrs idx -> do
      runCustomAbility attrs iid idx
      pure x
    -- What the event does when it is played: the common case, so it gets a
    -- place of its own rather than being written as a listener.
    PlayThisEvent iid (is attrs -> True) -> do
      runCustomSteps attrs iid "_onPlay"
      pure x
    _ -> do
      runCustomHandlers attrs msg
      CustomEvent <$> liftRunMessage msg attrs
