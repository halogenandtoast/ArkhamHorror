module Arkham.Types.Treachery.Cards.SordidAndSilent
  ( SordidAndSilent(..)
  , sordidAndSilent
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

newtype SordidAndSilent = SordidAndSilent TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sordidAndSilent :: TreacheryId -> a -> SordidAndSilent
sordidAndSilent uuid _ = SordidAndSilent $ baseAttrs uuid "02089"

instance HasModifiersFor env SordidAndSilent where
  getModifiersFor = noModifiersFor

instance HasActions env SordidAndSilent where
  getActions i window (SordidAndSilent attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env SordidAndSilent where
  runMessage msg t@(SordidAndSilent attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      lid <- getId @LocationId iid
      t <$ unshiftMessage (AttachTreachery treacheryId $ LocationTarget lid)
    EndRound -> case treacheryAttachedTarget of
      Just (LocationTarget lid) -> do
        iids <- getSetList @InvestigatorId lid
        t <$ unshiftMessages
          [ InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 1 | iid <- iids ]
      _ -> pure t
    AdvanceAgenda _ -> t <$ unshiftMessage (Discard $ toTarget attrs)
    _ -> SordidAndSilent <$> runMessage msg attrs
