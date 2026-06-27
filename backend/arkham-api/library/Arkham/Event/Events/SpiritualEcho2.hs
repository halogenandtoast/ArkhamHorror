module Arkham.Event.Events.SpiritualEcho2 (spiritualEcho2) where

import Arkham.Ability
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Trait (Trait (Ritual, Spell))
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype SpiritualEcho2 = SpiritualEcho2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spiritualEcho2 :: EventCard SpiritualEcho2
spiritualEcho2 = event SpiritualEcho2 Cards.spiritualEcho2

instance HasAbilities SpiritualEcho2 where
  getAbilities (SpiritualEcho2 a) = case a.attachedTo.location of
    Just lid ->
      [ restricted a 1 OwnsThis
          $ freeReaction
          $ ActivateAbility #after You
          $ PerformableAbility [AsIfAt lid]
          <> AssetAbility (hasAnyTrait [Spell, Ritual])
          <> oneOf [#action, #fast]
      ]
    Nothing -> []

toOriginalAbility :: [Window] -> Ability
toOriginalAbility [] = error "invalid window"
toOriginalAbility ((windowType -> Window.ActivateAbility _ _ ability) : _) = ability
toOriginalAbility (_ : xs) = toOriginalAbility xs

instance RunMessage SpiritualEcho2 where
  runMessage msg e@(SpiritualEcho2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      withLocationOf iid $ place attrs . AttachedToLocation
      pure e
    UseCardAbility iid (isSource attrs -> True) 1 ws@(toOriginalAbility -> ability) _ -> do
      -- Perform the echoed ability, then return Spiritual Echo to hand (printed order).
      -- While the copy resolves the event is still in play and the copied ability opens
      -- its own "after you activate" window; without a guard Spiritual Echo's reaction
      -- would be offered again on its own echo and queue a second ReturnToHand for an
      -- event that's already gone (crash, issue #4941). Suppressing this event's own
      -- ability for the duration keeps it single-use while preserving the order.
      case attrs.attachedTo.location of
        Just lid ->
          temporaryModifiers
            iid
            (attrs.ability 1)
            [AsIfAt lid, CannotTriggerAbilityMatching (AbilityIs (toSource attrs) 1)]
            do push $ UseAbility iid (ignoreActionCost ability) ws
        _ -> pure ()
      returnToHand iid attrs
      pure e
    _ -> SpiritualEcho2 <$> liftRunMessage msg attrs
