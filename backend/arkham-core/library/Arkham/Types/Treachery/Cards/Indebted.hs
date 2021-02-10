module Arkham.Types.Treachery.Cards.Indebted
  ( Indebted(..)
  , indebted
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
import Arkham.Types.Treachery.Helpers
import Arkham.Types.Treachery.Runner

newtype Indebted = Indebted TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

indebted :: TreacheryId -> Maybe InvestigatorId -> Indebted
indebted uuid iid = Indebted $ weaknessAttrs uuid iid "02037"

instance HasModifiersFor env Indebted where
  getModifiersFor _ (InvestigatorTarget iid) (Indebted attrs) =
    pure $ toModifiers
      attrs
      [ StartingResources (-2) | treacheryOnInvestigator iid attrs ]
  getModifiersFor _ _ _ = pure []

instance HasActions env Indebted where
  getActions iid window (Indebted attrs) = getActions iid window attrs

instance (TreacheryRunner env) => RunMessage env Indebted where
  runMessage msg t@(Indebted attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      t <$ unshiftMessage (AttachTreachery treacheryId $ InvestigatorTarget iid)
    _ -> Indebted <$> runMessage msg attrs
