module Arkham.Event.Cards.DenyExistence
  ( denyExistence
  , DenyExistence(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Card
import Arkham.Classes
import Arkham.Event.Runner
import Arkham.Message hiding (Discarded)
import Arkham.Target
import Arkham.Window

newtype DenyExistence = DenyExistence EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

denyExistence :: EventCard DenyExistence
denyExistence =
  event DenyExistence Cards.denyExistence

-- Discard cards from hand, lose resources, lose actions, take damage, or take horror.
--
-- So given the windows we need to figure out what is valid to ignore, and let
-- the player choose if multiple, we then push an effect for the current window
-- that targets that card, the aspect needs to be handled by the investigator,
-- or alternatively we remove the messages entirely, since this is a when, it
-- should be queued up, however we need to prequeue which is weird...

instance RunMessage DenyExistence where
  runMessage msg e@(DenyExistence attrs) = case msg of
    InvestigatorPlayEvent iid eid mTarget windows _ | eid == toId attrs -> do
      let
        go str w = Label str [ResolveEvent iid eid mTarget [w]]
        choices = flip mapMaybe windows $ \w -> case windowType w of
          Discarded{} -> Just $ go "discard cards" w
          LostResources{} -> Just $ go "lose resources" w
          LostActions{} -> Just $ go "lose actions" w
          WouldTakeDamage{} -> Just $ go "take damage" w
          WouldTakeHorror{} -> Just $ go "take horror" w
          _ -> Nothing
      push $ chooseOrRunOne iid choices
      pure e
    ResolveEvent _ eid _ [w] | eid == toId attrs -> do
      case windowType w of
        Discarded iid source c -> do
          popMessageMatching_ (== Do (DiscardCard iid source (toCardId c)))
        LostResources iid source n -> do
          popMessageMatching_ (== Do (LoseResources iid source n))
        LostActions iid source n -> do
          popMessageMatching_ (== Do (LoseActions iid source n))
        WouldTakeDamage _source (InvestigatorTarget iid) n -> do
          push $ CancelDamage iid n
        WouldTakeHorror _source (InvestigatorTarget iid) n -> do
          push $ CancelHorror iid n
        _ -> error "Invalid window"
      popMessageMatching_ $ \case
        CheckWindow _ [w'] -> windowType w == windowType w'
        _ -> False
      replaceMessageMatching
        \case
          CheckWindow _ ws -> any ((== windowType w) . windowType) ws
          _ -> False
        \case
          CheckWindow iids ws -> [CheckWindow iids $ filter ((/= windowType w) . windowType) ws]
          _ -> error "no match"
      pure e
    _ -> DenyExistence <$> runMessage msg attrs
