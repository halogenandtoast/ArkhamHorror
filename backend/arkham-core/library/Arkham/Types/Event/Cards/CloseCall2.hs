module Arkham.Types.Event.Cards.CloseCall2
  ( closeCall2
  , CloseCall2(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Timing qualified as Timing
import Arkham.Types.Window
import Arkham.Types.Window qualified as Window

newtype CloseCall2 = CloseCall2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

closeCall2 :: EventCard CloseCall2
closeCall2 = event CloseCall2 Cards.closeCall2

instance RunMessage env CloseCall2 where
  runMessage msg e@(CloseCall2 attrs) = case msg of
    InvestigatorPlayEvent _iid eid _ [Window Timing.After (Window.EnemyEvaded _ enemyId)]
      | eid == toId attrs
      -> e <$ pushAll
        [ ShuffleBackIntoEncounterDeck (EnemyTarget enemyId)
        , Discard (toTarget attrs)
        ]
    _ -> CloseCall2 <$> runMessage msg attrs
