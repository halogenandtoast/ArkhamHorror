module Arkham.Types.Event.Cards.Taunt2
  ( taunt2
  , Taunt2(..)
  )
where


import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner

newtype Taunt2 = Taunt2 EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

taunt2 :: InvestigatorId -> EventId -> Taunt2
taunt2 iid uuid = Taunt2 $ baseAttrs iid uuid "02019"

instance HasActions env Taunt2 where
  getActions iid window (Taunt2 attrs) = getActions iid window attrs

instance HasModifiersFor env Taunt2 where
  getModifiersFor = noModifiersFor

instance (EventRunner env) => RunMessage env Taunt2 where
  runMessage msg e@(Taunt2 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      lid <- getId @LocationId iid
      enemyIds <- getSetList lid
      e <$ unshiftMessage
        (chooseSome
          iid
          [ TargetLabel
              (EnemyTarget enemyId)
              [EngageEnemy iid enemyId False, DrawCards iid 1 False]
          | enemyId <- enemyIds
          ]
        )
    _ -> Taunt2 <$> runMessage msg attrs
