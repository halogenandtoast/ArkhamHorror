-- | The runner behind a debug-authored custom event. See "Arkham.Custom.Enemy".
module Arkham.Custom.Event (CustomEvent (..), customEvent) where

import Arkham.Card.CardDef (CardDef)
import Arkham.Custom.Ability (
  customAbilities,
  customModifiers,
  isCustomAbility,
  runCustomAbility,
  runCustomHandlers,
  runCustomRevelation,
  runCustomSteps,
  pattern ZonedUseThisAbility,
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
    ZonedUseThisAbility iid (isSource attrs -> True) idx ws payment | isCustomAbility attrs idx -> do
      runCustomAbility attrs iid idx ws payment
      pure x
    -- What the event does when it is played: the common case, so it gets a
    -- place of its own rather than being written as a listener.
    PlayThisEvent iid (is attrs -> True) -> do
      runCustomSteps attrs iid "_onPlay"
      pure x
    -- What it does when it is revealed. Not an ability: no one activates it, and
    -- the card may have to place itself before the engine tidies it away.
    Revelation iid (isSource attrs -> True) -> do
      runCustomRevelation attrs iid
      runCustomHandlers attrs msg
      CustomEvent <$> liftRunMessage msg attrs
    _ -> do
      runCustomHandlers attrs msg
      CustomEvent <$> liftRunMessage msg attrs
