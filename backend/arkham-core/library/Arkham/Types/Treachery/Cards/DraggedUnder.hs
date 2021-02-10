module Arkham.Types.Treachery.Cards.DraggedUnder where

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

newtype DraggedUnder = DraggedUnder TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

draggedUnder :: TreacheryId -> a -> DraggedUnder
draggedUnder uuid _ = DraggedUnder $ baseAttrs uuid "81026"

instance HasModifiersFor env DraggedUnder where
  getModifiersFor = noModifiersFor

instance HasActions env DraggedUnder where
  getActions i window (DraggedUnder attrs) = getActions i window attrs

instance (TreacheryRunner env) => RunMessage env DraggedUnder where
  runMessage msg t@(DraggedUnder attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ unshiftMessage (RevelationSkillTest iid source SkillAgility 3)
    MoveFrom iid _ | treacheryOnInvestigator iid attrs -> t <$ unshiftMessages
      [ InvestigatorAssignDamage iid (TreacherySource treacheryId) DamageAny 2 0
      , Discard (TreacheryTarget treacheryId)
      ]
    EndTurn iid | treacheryOnInvestigator iid attrs -> t <$ unshiftMessage
      (BeginSkillTest
        iid
        (TreacherySource treacheryId)
        (InvestigatorTarget iid)
        Nothing
        SkillAgility
        3
      )
    FailedSkillTest iid _ source _ _ _ | isSource attrs source -> t <$ when
      (isNothing treacheryAttachedTarget)
      (unshiftMessage $ AttachTreachery treacheryId (InvestigatorTarget iid))
    PassedSkillTest _ _ source _ _ _ | isSource attrs source ->
      t <$ unshiftMessage (Discard $ toTarget attrs)
    _ -> DraggedUnder <$> runMessage msg attrs
