module Arkham.Event.Cards.DenyExistence
  ( denyExistence
  , DenyExistence(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Classes
import Arkham.Event.Runner
import Arkham.Message

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
    InvestigatorPlayEvent _ eid _ _ _ | eid == toId attrs -> do
      e <$ pushAll [discard attrs]
    _ -> DenyExistence <$> runMessage msg attrs
