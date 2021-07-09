module Arkham.Types.Event.Cards.CloseCall2
  ( closeCall2
  , CloseCall2(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
-- import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
-- import Arkham.Types.Id
import Arkham.Types.Message
-- import Arkham.Types.Trait
import Arkham.Types.Window

newtype CloseCall2 = CloseCall2 EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

closeCall2 :: EventCard CloseCall2
closeCall2 = event CloseCall2 Cards.closeCall2

instance HasModifiersFor env CloseCall2 where
  getModifiersFor = noModifiersFor

instance HasActions env CloseCall2 where
  getActions i window (CloseCall2 attrs) = getActions i window attrs

instance RunMessage env CloseCall2 where
  runMessage msg e@(CloseCall2 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ windows | eid == toId attrs -> do
      let
        enemyTargets = flip
          mapMaybe
          windows
          \window -> case windowType window of
            AfterEnemyEvaded You _ -> windowTarget window
            _ -> Nothing
      e <$ pushAll
        [ chooseOne
          iid
          [ TargetLabel target [ShuffleBackIntoEncounterDeck target]
          | target <- enemyTargets
          ]
        , Discard (toTarget attrs)
        ]
    _ -> CloseCall2 <$> runMessage msg attrs
