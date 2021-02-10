module Arkham.Types.Event.Cards.EmergencyAid
  ( emergencyAid
  , EmergencyAid(..)
  ) where

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
import Arkham.Types.Trait

newtype EmergencyAid = EmergencyAid EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

emergencyAid :: InvestigatorId -> EventId -> EmergencyAid
emergencyAid iid uuid = EmergencyAid $ baseAttrs iid uuid "02105"

instance HasActions env EmergencyAid where
  getActions iid window (EmergencyAid attrs) = getActions iid window attrs

instance HasModifiersFor env EmergencyAid where
  getModifiersFor = noModifiersFor

instance
  ( HasQueue env
  , HasId LocationId env InvestigatorId
  , HasSet InvestigatorId env LocationId
  , HasSet AssetId env (InvestigatorId, [Trait])
  )
  => RunMessage env EmergencyAid where
  runMessage msg e@(EmergencyAid attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      locationId <- getId @LocationId iid
      investigatorIds <- getSetList locationId
      let investigatorTargets = map InvestigatorTarget investigatorIds
      allyTargets <- map AssetTarget . concat <$> for
        investigatorIds
        (getSetList . (, [Ally]))
      e <$ unshiftMessage
        (chooseOne
          iid
          [ TargetLabel target [HealDamage target 2]
          | target <- investigatorTargets <> allyTargets
          ]
        )
    _ -> EmergencyAid <$> runMessage msg attrs
