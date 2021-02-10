module Arkham.Types.Treachery.Cards.ChillFromBelow where

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

newtype ChillFromBelow = ChillFromBelow TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chillFromBelow :: TreacheryId -> a -> ChillFromBelow
chillFromBelow uuid _ = ChillFromBelow $ baseAttrs uuid "50040"

instance HasModifiersFor env ChillFromBelow where
  getModifiersFor = noModifiersFor

instance HasActions env ChillFromBelow where
  getActions i window (ChillFromBelow attrs) = getActions i window attrs

instance (TreacheryRunner env) => RunMessage env ChillFromBelow where
  runMessage msg t@(ChillFromBelow attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> t <$ unshiftMessages
      [ RevelationSkillTest iid source SkillWillpower 3
      , Discard (TreacheryTarget treacheryId)
      ]
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ n
      | isSource attrs source -> do
        handCount <- unCardCount <$> getCount iid
        if handCount < n
          then
            unshiftMessages
            $ replicate handCount (RandomDiscard iid)
            <> [InvestigatorAssignDamage iid source DamageAny (n - handCount) 0]
          else unshiftMessages $ replicate n (RandomDiscard iid)
        pure t
    _ -> ChillFromBelow <$> runMessage msg attrs
