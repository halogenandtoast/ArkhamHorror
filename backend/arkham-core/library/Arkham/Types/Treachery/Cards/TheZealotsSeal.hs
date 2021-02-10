module Arkham.Types.Treachery.Cards.TheZealotsSeal where

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


import Arkham.Types.Game.Helpers
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype TheZealotsSeal = TheZealotsSeal TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theZealotsSeal :: TreacheryId -> a -> TheZealotsSeal
theZealotsSeal uuid _ = TheZealotsSeal $ baseAttrs uuid "50024"

instance HasModifiersFor env TheZealotsSeal where
  getModifiersFor = noModifiersFor

instance HasActions env TheZealotsSeal where
  getActions i window (TheZealotsSeal attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env TheZealotsSeal where
  runMessage msg t@(TheZealotsSeal attrs@TreacheryAttrs {..}) = case msg of
    Revelation _ source | isSource attrs source -> do
      investigatorIds <- getInvestigatorIds
      -- we must unshift this first for other effects happen before
      unshiftMessage (Discard $ TreacheryTarget treacheryId)
      t <$ for_
        investigatorIds
        (\iid' -> do
          handCardCount <- unCardCount <$> getCount iid'
          if handCardCount <= 3
            then unshiftMessage
              (InvestigatorAssignDamage iid' (toSource attrs) DamageAny 1 1)
            else unshiftMessage
              (RevelationSkillTest iid' source SkillWillpower 2)
        )
    FailedSkillTest iid _ (TreacherySource tid) SkillTestInitiatorTarget{} _ _
      | tid == treacheryId -> t
      <$ unshiftMessages [RandomDiscard iid, RandomDiscard iid]
    _ -> TheZealotsSeal <$> runMessage msg attrs
