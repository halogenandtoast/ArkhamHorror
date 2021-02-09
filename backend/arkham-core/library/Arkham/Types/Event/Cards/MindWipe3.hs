module Arkham.Types.Event.Cards.MindWipe3 where


import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Trait

newtype MindWipe3 = MindWipe3 EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mindWipe3 :: InvestigatorId -> EventId -> MindWipe3
mindWipe3 iid uuid = MindWipe3 $ baseAttrs iid uuid "50008"

instance HasModifiersFor env MindWipe3 where
  getModifiersFor = noModifiersFor

instance HasActions env MindWipe3 where
  getActions i window (MindWipe3 attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env MindWipe3 where
  runMessage msg e@(MindWipe3 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      locationId <- getId @LocationId iid
      enemyIds <- getSetList locationId
      nonEliteEnemyIds <- flip filterM enemyIds $ \enemyId -> do
        notElem Elite <$> getSet enemyId
      if null nonEliteEnemyIds
        then e <$ unshiftMessage (Discard (EventTarget eventId))
        else e <$ unshiftMessages
          [ chooseOne
            iid
            [ TargetLabel
                (EnemyTarget eid')
                [CreateEffect "" Nothing (toSource attrs) (EnemyTarget eid')]
            | eid' <- nonEliteEnemyIds
            ]
          , Discard (EventTarget eid)
          ]
    _ -> MindWipe3 <$> runMessage msg attrs
