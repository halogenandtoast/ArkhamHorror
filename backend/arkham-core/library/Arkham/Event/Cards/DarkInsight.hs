module Arkham.Event.Cards.DarkInsight (
  darkInsight,
  DarkInsight (..),
)
where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Window
import Arkham.Window qualified as Window

newtype DarkInsight = DarkInsight EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

darkInsight :: EventCard DarkInsight
darkInsight =
  event DarkInsight Cards.darkInsight

instance RunMessage DarkInsight where
  runMessage msg e@(DarkInsight attrs) = case msg of
    InvestigatorPlayEvent _ eid _ [(windowType -> Window.DrawCard _ card _)] _ | eid == toId attrs -> do
      let
        cancelMsg = case toCardType card of
          TreacheryType -> CancelNext (toSource attrs) RevelationMessage
          EnemyType -> CancelNext (toSource attrs) DrawEnemyMessage
          _ -> error "Not handled yet"
      -- TODO: Figure out what exactly we should cancel (RevelationMessage for treachery, Enemy, Event, Skill, etc.)
      e <$ push cancelMsg
    _ -> DarkInsight <$> runMessage msg attrs
