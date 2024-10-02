module Arkham.Event.Events.DenyExistence (denyExistence, DenyExistence (..)) where

import Arkham.Classes.HasQueue (popMessageMatching_, replaceMessageMatching)
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted hiding (Discarded)
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
      let
        go str w = Label str [ResolveEvent iid eid mTarget [w]]
        choices = flip mapMaybe windows $ \w -> case windowType w of
          WouldDiscardFromHand {} -> Just $ go "discard cards" w
          LostResources {} -> Just $ go "lose resources" w
          LostActions {} -> Just $ go "lose actions" w
          WouldTakeDamage {} -> Just $ go "take damage" w
          WouldTakeHorror {} -> Just $ go "take horror" w
          _ -> Nothing
      chooseOrRunOne iid choices
      pure e
    ResolveEvent _ eid _ [w] | eid == toId attrs -> do
      lift $ case windowType w of
        WouldDiscardFromHand iid source -> do
          popMessageMatching_ \case
            Do (DiscardFromHand handDiscard) -> handDiscard.investigator == iid && handDiscard.source == toSource source
            _ -> False
        LostResources iid source n -> do
          popMessageMatching_ (== Do (LoseResources iid source n))
        LostActions iid source n -> do
          popMessageMatching_ (== Do (LoseActions iid source n))
        WouldTakeDamage _source (InvestigatorTarget iid) n _ -> do
          push $ CancelDamage iid n
        WouldTakeHorror _source (InvestigatorTarget iid) n -> do
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
