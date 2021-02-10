module Arkham.Types.Treachery.Cards.CurseOfTheRougarou
  ( CurseOfTheRougarou(..)
  , curseOfTheRougarou
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


import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype Metadata = Metadata { dealtDamageThisTurn :: Bool }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype CurseOfTheRougarou = CurseOfTheRougarou (TreacheryAttrs `With` Metadata)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

curseOfTheRougarou :: TreacheryId -> Maybe InvestigatorId -> CurseOfTheRougarou
curseOfTheRougarou uuid iid =
  CurseOfTheRougarou . (`with` Metadata False) $ weaknessAttrs uuid iid "81029"

instance HasModifiersFor env CurseOfTheRougarou where
  getModifiersFor = noModifiersFor

instance HasActions env CurseOfTheRougarou where
  getActions iid window (CurseOfTheRougarou (attrs `With` _)) =
    getActions iid window attrs

instance (TreacheryRunner env) => RunMessage env CurseOfTheRougarou where
  runMessage msg t@(CurseOfTheRougarou (attrs@TreacheryAttrs {..} `With` metadata)) =
    case msg of
      Revelation iid source | isSource attrs source -> do
        t <$ unshiftMessage
          (AttachTreachery treacheryId $ InvestigatorTarget iid)
      EnemyDamage _ iid _ n | treacheryOnInvestigator iid attrs && n > 0 ->
        CurseOfTheRougarou . (`with` Metadata True) <$> runMessage msg attrs
      InvestigatorAssignDamage _ (InvestigatorSource iid) _ n 0
        | treacheryOnInvestigator iid attrs && n > 0
        -> CurseOfTheRougarou . (`with` Metadata True) <$> runMessage msg attrs
      EndTurn iid | treacheryOnInvestigator iid attrs -> do
        unless
          (dealtDamageThisTurn metadata)
          (unshiftMessage
          $ InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 1
          )
        CurseOfTheRougarou . (`with` Metadata False) <$> runMessage msg attrs
      _ -> CurseOfTheRougarou . (`with` metadata) <$> runMessage msg attrs
