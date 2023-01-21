module Arkham.Event.Cards.DarkInsight
  ( darkInsight
  , DarkInsight(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Card
import Arkham.Classes
import Arkham.Event.Runner
import Arkham.Message
import Arkham.Window
import Arkham.Window qualified as Window

newtype DarkInsight = DarkInsight EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkInsight :: EventCard DarkInsight
darkInsight =
  event DarkInsight Cards.darkInsight

instance RunMessage DarkInsight where
  runMessage msg e@(DarkInsight attrs) = case msg of
    InvestigatorPlayEvent _ eid _ [Window _ (Window.DrawCard _ card _)] _ | eid == toId attrs -> do
      let
        cancelMsg = case toCardType card of
          TreacheryType -> CancelNext (toSource attrs) RevelationMessage
          EnemyType -> CancelNext (toSource attrs) DrawEnemyMessage
          _ -> error "Not handled yet"
      -- TODO: Figure out what exactly we should cancel (RevelationMessage for treachery, Enemy, Event, Skill, etc.)
      e <$ pushAll [cancelMsg, Discard (toTarget attrs)]
    _ -> DarkInsight <$> runMessage msg attrs
