module Arkham.Types.Event.Cards.Lucky2 where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.CampaignId
import Arkham.Types.Card
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.Effect.Window
import Arkham.Types.EffectId
import Arkham.Types.EffectMetadata
import Arkham.Types.EncounterSet (EncounterSet)
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.Exception
import Arkham.Types.GameValue
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.ScenarioId
import Arkham.Types.SkillId
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Stats (Stats)
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.TreacheryId
import Arkham.Types.Window


import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner

newtype Lucky2 = Lucky2 EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lucky2 :: InvestigatorId -> EventId -> Lucky2
lucky2 iid uuid = Lucky2 $ baseAttrs iid uuid "01084"

instance HasModifiersFor env Lucky2 where
  getModifiersFor = noModifiersFor

instance HasActions env Lucky2 where
  getActions i window (Lucky2 attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env Lucky2 where
  runMessage msg e@(Lucky2 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> e <$ unshiftMessages
      [ Discard (EventTarget eid)
      , DrawCards iid 1 False
      , CreateEffect "01084" Nothing (EventSource eid) (InvestigatorTarget iid)
      ]
    _ -> Lucky2 <$> runMessage msg attrs
