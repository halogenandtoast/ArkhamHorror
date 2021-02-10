module Arkham.Types.Event.Cards.HotStreak4 where

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

newtype HotStreak4 = HotStreak4 EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hotStreak4 :: InvestigatorId -> EventId -> HotStreak4
hotStreak4 iid uuid = HotStreak4 $ baseAttrs iid uuid "01057"

instance HasModifiersFor env HotStreak4 where
  getModifiersFor = noModifiersFor

instance HasActions env HotStreak4 where
  getActions i window (HotStreak4 attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env HotStreak4 where
  runMessage msg e@(HotStreak4 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId ->
      e <$ unshiftMessages
        [TakeResources iid 10 False, Discard (EventTarget eid)]
    _ -> HotStreak4 <$> runMessage msg attrs
