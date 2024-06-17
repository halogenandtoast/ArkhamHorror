module Arkham.Event.Cards.DenyExistence5 (
  denyExistence5,
  DenyExistence5 (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner hiding (Discarded)
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
  runMessage msg e@(DenyExistence5 attrs) = case msg of
    InvestigatorPlayEvent iid eid mTarget windows _ | eid == toId attrs -> do
      let
        go str w = Label str [ResolveEvent iid eid mTarget [w]]
        choices = flip mapMaybe windows $ \w -> case windowType w of
          Discarded {} -> Just $ go "discard cards" w
          LostResources {} -> Just $ go "lose resources" w
          LostActions {} -> Just $ go "lose actions" w
          WouldTakeDamage {} -> Just $ go "take damage" w
          WouldTakeHorror {} -> Just $ go "take horror" w
          _ -> Nothing
      player <- getPlayer iid
      push $ chooseOrRunOne player choices
      pure e
    ResolveEvent _ eid _ [w] | eid == toId attrs -> do
      case windowType w of
        Discarded iid source c -> do
          let drawing = drawCards iid source 1
          replaceMessageMatching (== Do (toMessage $ discardCard iid source c))
            $ \_ -> [drawing]
        LostResources iid source n -> do
          replaceMessageMatching (== Do (LoseResources iid source n))
            $ \_ -> [TakeResources iid n source False]
        LostActions iid source n -> do
          replaceMessageMatching (== Do (LoseActions iid source n))
            $ \_ -> [GainActions iid source n]
        WouldTakeDamage source (InvestigatorTarget iid) n -> do
          pushAll
            [CancelDamage iid n, HealDamage (InvestigatorTarget iid) source n]
        WouldTakeHorror source (InvestigatorTarget iid) n -> do
          pushAll
            [CancelHorror iid n, HealHorror (InvestigatorTarget iid) source n]
        _ -> error "Invalid window"
      popMessageMatching_ $ \case
        CheckWindow _ [w'] -> windowType w == windowType w'
        _ -> False
      replaceMessageMatching
        \case
          CheckWindow _ ws -> any ((== windowType w) . windowType) ws
          _ -> False
        \case
          CheckWindow iids ws ->
            [CheckWindow iids $ filter ((/= windowType w) . windowType) ws]
          _ -> error "no match"
      pure e
    _ -> DenyExistence5 <$> runMessage msg attrs
