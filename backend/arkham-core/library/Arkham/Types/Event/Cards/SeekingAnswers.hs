module Arkham.Types.Event.Cards.SeekingAnswers
  ( seekingAnswers
  , SeekingAnswers(..)
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

newtype SeekingAnswers = SeekingAnswers EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

seekingAnswers :: InvestigatorId -> EventId -> SeekingAnswers
seekingAnswers iid uuid = SeekingAnswers $ baseAttrs iid uuid "02023"

instance HasActions env SeekingAnswers where
  getActions iid window (SeekingAnswers attrs) = getActions iid window attrs

instance HasModifiersFor env SeekingAnswers where
  getModifiersFor = noModifiersFor

instance (HasQueue env, HasId LocationId env InvestigatorId) => RunMessage env SeekingAnswers where
  runMessage msg e@(SeekingAnswers attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      lid <- getId @LocationId iid
      e <$ unshiftMessages
        [ CreateEffect
          "02023"
          Nothing
          (toSource attrs)
          (InvestigationTarget iid lid)
        , Discard (toTarget attrs)
        ]
    _ -> SeekingAnswers <$> runMessage msg attrs
