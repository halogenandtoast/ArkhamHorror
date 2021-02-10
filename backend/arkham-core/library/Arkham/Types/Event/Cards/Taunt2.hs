module Arkham.Types.Event.Cards.Taunt2
  ( taunt2
  , Taunt2(..)
  )
where

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
