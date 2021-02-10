module Arkham.Types.Treachery.Cards.EagerForDeath
  ( EagerForDeath(..)
  , eagerForDeath
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


import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype EagerForDeath = EagerForDeath TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eagerForDeath :: TreacheryId -> a -> EagerForDeath
eagerForDeath uuid _ = EagerForDeath $ baseAttrs uuid "02091"

instance HasModifiersFor env EagerForDeath where
  getModifiersFor = noModifiersFor

instance HasActions env EagerForDeath where
  getActions i window (EagerForDeath attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env EagerForDeath where
  runMessage msg t@(EagerForDeath attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      difficulty <- (+ 2) . unDamageCount <$> getCount iid
      t <$ unshiftMessages
        [ RevelationSkillTest iid source SkillWillpower difficulty
        , Discard (toTarget attrs)
        ]
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> t
      <$ unshiftMessage (InvestigatorAssignDamage iid source DamageAny 0 2)
    _ -> EagerForDeath <$> runMessage msg attrs
