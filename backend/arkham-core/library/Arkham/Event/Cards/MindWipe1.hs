module Arkham.Event.Cards.MindWipe1
  ( mindWipe1
  , MindWipe1(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.Event.Attrs
import Arkham.Event.Runner
import Arkham.Id
import Arkham.Message
import Arkham.Source
import Arkham.Target
import Arkham.Trait

newtype MindWipe1 = MindWipe1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mindWipe1 :: EventCard MindWipe1
mindWipe1 = event MindWipe1 Cards.mindWipe1

instance EventRunner env => RunMessage MindWipe1 where
  runMessage msg e@(MindWipe1 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      locationId <- getId @LocationId iid
      enemyIds <- getSetList locationId
      nonEliteEnemyIds <- flip filterM enemyIds $ \enemyId -> do
        notElem Elite <$> getSet enemyId
      if null nonEliteEnemyIds
        then e <$ push (Discard (EventTarget eventId))
        else e <$ pushAll
          [ chooseOne
            iid
            [ TargetLabel
                (EnemyTarget eid')
                [ CreateEffect
                    "01068"
                    Nothing
                    (EventSource eventId)
                    (EnemyTarget eid')
                ]
            | eid' <- nonEliteEnemyIds
            ]
          , Discard (EventTarget eid)
          ]
    _ -> MindWipe1 <$> runMessage msg attrs
