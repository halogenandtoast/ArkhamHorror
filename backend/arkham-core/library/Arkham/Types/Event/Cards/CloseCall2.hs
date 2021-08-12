module Arkham.Types.Event.Cards.CloseCall2
  ( closeCall2
  , CloseCall2(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Message
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Window
import qualified Arkham.Types.Window as W

newtype CloseCall2 = CloseCall2 EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

closeCall2 :: EventCard CloseCall2
closeCall2 = event CloseCall2 Cards.closeCall2

instance HasModifiersFor env CloseCall2
instance HasActions CloseCall2

instance RunMessage env CloseCall2 where
  runMessage msg e@(CloseCall2 attrs) = case msg of
    InvestigatorPlayEvent _iid eid _ [Window Timing.After (W.EnemyEvaded _ enemyId)]
      | eid == toId attrs
      -> e <$ pushAll
        [ ShuffleBackIntoEncounterDeck (EnemyTarget enemyId)
        , Discard (toTarget attrs)
        ]
    _ -> CloseCall2 <$> runMessage msg attrs
