module Arkham.Types.Treachery.Cards.InsatiableBloodlust where

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
import Arkham.Types.Treachery.Helpers
import Arkham.Types.Treachery.Runner

newtype InsatiableBloodlust = InsatiableBloodlust TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

insatiableBloodlust :: TreacheryId -> a -> InsatiableBloodlust
insatiableBloodlust uuid _ = InsatiableBloodlust $ baseAttrs uuid "81036"

instance HasModifiersFor env InsatiableBloodlust where
  getModifiersFor _ (EnemyTarget eid) (InsatiableBloodlust attrs)
    | treacheryOnEnemy eid attrs = pure $ toModifiers
      attrs
      [EnemyFight 1, DamageDealt 1, HorrorDealt 1, CannotBeEvaded]
  getModifiersFor _ _ _ = pure []

instance HasActions env InsatiableBloodlust where
  getActions i window (InsatiableBloodlust attrs) = getActions i window attrs

instance (TreacheryRunner env) => RunMessage env InsatiableBloodlust where
  runMessage msg t@(InsatiableBloodlust attrs@TreacheryAttrs {..}) = case msg of
    Revelation _iid source | isSource attrs source -> do
      mrougarou <- fmap unStoryEnemyId <$> getId (CardCode "81028")
      case mrougarou of
        Nothing -> error "can't happen"
        Just eid -> do
          unshiftMessage (AttachTreachery treacheryId (EnemyTarget eid))
      InsatiableBloodlust <$> runMessage msg attrs
    EnemyDamage eid _ _ n | n > 0 -> do
      mrougarou <- fmap unStoryEnemyId <$> getId (CardCode "81028")
      t <$ when
        (mrougarou == Just eid)
        (unshiftMessage (Discard $ toTarget attrs))
    _ -> InsatiableBloodlust <$> runMessage msg attrs
