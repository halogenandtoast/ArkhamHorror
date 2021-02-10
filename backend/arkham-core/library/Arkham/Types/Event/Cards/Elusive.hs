module Arkham.Types.Event.Cards.Elusive where

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

newtype Elusive = Elusive EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

elusive :: InvestigatorId -> EventId -> Elusive
elusive iid uuid = Elusive $ baseAttrs iid uuid "01050"

instance HasModifiersFor env Elusive where
  getModifiersFor = noModifiersFor

instance HasActions env Elusive where
  getActions i window (Elusive attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env Elusive where
  runMessage msg e@(Elusive attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      enemyIds <- getSetList iid
      emptyLocations <- mapSet unEmptyLocationId <$> getSet ()
      revealedLocations <- mapSet unRevealedLocationId <$> getSet ()
      let
        candidateLocations =
          setToList $ emptyLocations `intersection` revealedLocations

      e <$ unshiftMessages
        ([ DisengageEnemy iid enemyId | enemyId <- enemyIds ]
        <> [ chooseOne iid [ MoveTo iid lid | lid <- candidateLocations ]
           | not (null candidateLocations)
           ]
        <> [Discard (EventTarget eventId)]
        )
    _ -> Elusive <$> runMessage msg attrs
