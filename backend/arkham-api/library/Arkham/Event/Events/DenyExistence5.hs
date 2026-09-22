module Arkham.Event.Events.DenyExistence5 (denyExistence5, DenyExistence5 (..)) where

import Arkham.Classes.HasQueue (popMessageMatching_, replaceMessageMatching)
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted hiding (Discarded, drawCards)
import Arkham.Helpers.Cost (cancelCostPaymentFrom)
import Arkham.Helpers.Message (drawCards)
import Arkham.I18n
import Arkham.Window

newtype DenyExistence5 = DenyExistence5 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

denyExistence5 :: EventCard DenyExistence5
denyExistence5 = event DenyExistence5 Cards.denyExistence5

-- Discard cards from hand, lose resources, lose actions, take damage, or take horror.
--
-- So given the windows we need to figure out what is valid to ignore, and let
-- the player choose if multiple, we then push an effect for the current window
-- that targets that card, the aspect needs to be handled by the investigator,
-- or alternatively we remove the messages entirely, since this is a when, it
-- should be queued up, however we need to prequeue which is weird...

instance RunMessage DenyExistence5 where
  runMessage msg e@(DenyExistence5 attrs) = runQueueT $ case msg of
    InvestigatorPlayEvent iid eid mTarget windows _ | eid == toId attrs -> do
      let resolve w = push $ ResolveEvent iid eid mTarget [w]
      chooseOrRunOneM iid $ cardI18n $ scope "denyExistence5" $ for_ windows \w -> case windowType w of
        WouldDiscardFromHand {} -> labeled "drawInstead" $ resolve w
        LostResources _ _ n -> countVar n $ labeled "gainResourcesInstead" $ resolve w
        LostActions _ _ n -> countVar n $ labeled "gainActionsInstead" $ resolve w
        WouldTakeDamage _ _ n _ -> countVar n $ labeled "healDamageInstead" $ resolve w
        WouldTakeHorror _ _ n -> countVar n $ labeled "healHorrorInstead" $ resolve w
        _ -> pure ()
      pure e
    ResolveEvent _ eid _ [w] | eid == toId attrs -> do
      lift $ case windowType w of
        WouldDiscardFromHand iid source -> do
          cancelCostPaymentFrom source
          let drawing = drawCards iid source
          replaceMessageMatching
            \case
              Do (DiscardFromHand handDiscard) -> handDiscard.investigator == iid && handDiscard.source == toSource source
              _ -> False
            \case
              Do (DiscardFromHand handDiscard) -> [drawing handDiscard.amount]
              _ -> []
        LostResources iid source n -> do
          cancelCostPaymentFrom source
          replaceMessageMatching (== Do (LoseResources iid source n))
            $ \_ -> [TakeResources iid n source False]
        LostActions iid source n -> do
          cancelCostPaymentFrom source
          replaceMessageMatching (== Do (LoseActions iid source n))
            $ \_ -> [GainActions iid source n]
        WouldTakeDamage source (InvestigatorTarget iid) n _ -> do
          cancelCostPaymentFrom source
          pushAll
            [CancelDamage iid n, HealDamage (InvestigatorTarget iid) source n]
        WouldTakeHorror source (InvestigatorTarget iid) n -> do
          cancelCostPaymentFrom source
          pushAll
            [CancelHorror iid n, HealHorror (InvestigatorTarget iid) source n]
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
    _ -> DenyExistence5 <$> liftRunMessage msg attrs
