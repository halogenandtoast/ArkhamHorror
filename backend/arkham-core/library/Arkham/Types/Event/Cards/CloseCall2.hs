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
import Arkham.Types.Target
-- import Arkham.Types.Trait
-- import Arkham.Types.Window

newtype CloseCall2 = CloseCall2 EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

closeCall2 :: EventCard CloseCall2
closeCall2 = event CloseCall2 Cards.closeCall2

instance HasModifiersFor env CloseCall2 where
  getModifiersFor = noModifiersFor

instance HasActions env CloseCall2 where
-- instance (GetCardDef env EnemyId, HasSet Trait env EnemyId) => HasActions env CloseCall2 where
  -- getActions iid (InHandWindow ownerId (AfterEnemyEvaded You eid)) (CloseCall2 attrs)
  --   | iid == ownerId
  --   = do
  --     traits' <- getSet eid
  --     cardDef <- getCardDef eid
  --     pure
  --       [ InitiatePlayCard iid (toCardId attrs) (Just $ EnemyTarget eid) False
  --       | Elite `notMember` traits' && not (cdWeakness cardDef)
  --       ]
  getActions i window (CloseCall2 attrs) = getActions i window attrs

instance RunMessage env CloseCall2 where
  runMessage msg e@(CloseCall2 attrs) = case msg of
    InvestigatorPlayEvent _iid eid (Just (EnemyTarget enemyId))
      | eid == toId attrs -> e <$ pushAll
        [ ShuffleBackIntoEncounterDeck (EnemyTarget enemyId)
        , Discard (toTarget attrs)
        ]
    _ -> CloseCall2 <$> runMessage msg attrs
