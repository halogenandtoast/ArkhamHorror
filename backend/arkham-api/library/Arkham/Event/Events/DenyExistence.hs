module Arkham.Event.Events.DenyExistence (denyExistence, DenyExistence (..)) where

import Arkham.Classes.HasQueue (popMessageMatching_, replaceMessageMatching)
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted hiding (Discarded)
import Arkham.Helpers.Cost (cancelCostPaymentFrom)
import Arkham.I18n
import Arkham.Window

newtype DenyExistence = DenyExistence EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

denyExistence :: EventCard DenyExistence
denyExistence = event DenyExistence Cards.denyExistence

-- Discard cards from hand, lose resources, lose actions, take damage, or take horror.
--
-- So given the windows we need to figure out what is valid to ignore, and let
-- the player choose if multiple, we then push an effect for the current window
-- that targets that card, the aspect needs to be handled by the investigator,
-- or alternatively we remove the messages entirely, since this is a when, it
-- should be queued up, however we need to prequeue which is weird...

instance RunMessage DenyExistence where
  runMessage msg e@(DenyExistence attrs) = runQueueT $ case msg of
    InvestigatorPlayEvent iid eid mTarget windows _ | eid == toId attrs -> do
      let resolve w = push $ ResolveEvent iid eid mTarget [w]
      chooseOrRunOneM iid $ cardI18n $ scope "denyExistence" $ for_ windows \w -> case windowType w of
        WouldDiscardFromHand {} -> labeled "cancelDiscardFromHand" $ resolve w
        LostResources _ _ n -> countVar n $ labeled "cancelLoseResources" $ resolve w
        LostActions _ _ n -> countVar n $ labeled "cancelLoseActions" $ resolve w
        WouldTakeDamage _ _ n _ -> unscoped $ countVar n $ labeled "cancelDamage" $ resolve w
        WouldTakeHorror _ _ n -> unscoped $ countVar n $ labeled "cancelHorror" $ resolve w
        _ -> pure ()
      pure e
    ResolveEvent _ eid _ [w] | eid == toId attrs -> do
      cancelWindowBatch [w]
      lift $ case windowType w of
        WouldDiscardFromHand iid source -> do
          cancelCostPaymentFrom source
          popMessageMatching_ \case
            Do (DiscardFromHand handDiscard) -> handDiscard.investigator == iid && handDiscard.source == toSource source
            _ -> False
        LostResources iid source n -> do
          cancelCostPaymentFrom source
          popMessageMatching_ (== Do (LoseResources iid source n))
        LostActions iid source n -> do
          cancelCostPaymentFrom source
          popMessageMatching_ (== Do (LoseActions iid source n))
        WouldTakeDamage source (InvestigatorTarget iid) n _ -> do
          cancelCostPaymentFrom source
          push $ CancelDamage iid n
        WouldTakeHorror source (InvestigatorTarget iid) n -> do
          cancelCostPaymentFrom source
          push $ CancelHorror iid n
        _ -> error "Invalid window"
      popMessageMatching_ $ \case
        CheckWindows [w'] -> windowType w == windowType w'
        Do (CheckWindows [w']) -> windowType w == windowType w'
        _ -> False
      replaceMessageMatching
        \case
          CheckWindows ws -> any ((== windowType w) . windowType) ws
          Do (CheckWindows ws) -> any ((== windowType w) . windowType) ws
          _ -> False
        \case
          CheckWindows ws ->
            [CheckWindows $ filter ((/= windowType w) . windowType) ws]
          Do (CheckWindows ws) ->
            [Do (CheckWindows $ filter ((/= windowType w) . windowType) ws)]
          _ -> error "no match"
      cancelledOrIgnoredCardOrGameEffect attrs
      pure e
    _ -> DenyExistence <$> liftRunMessage msg attrs
