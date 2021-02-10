module Arkham.Types.Asset.Cards.ArcaneEnlightenment
  ( ArcaneEnlightenment(..)
  , arcaneEnlightenment
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


import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Trait

newtype ArcaneEnlightenment = ArcaneEnlightenment AssetAttrs
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

arcaneEnlightenment :: AssetId -> ArcaneEnlightenment
arcaneEnlightenment uuid =
  ArcaneEnlightenment $ (baseAttrs uuid "60205") { assetSlots = [ArcaneSlot] }

instance HasModifiersFor env ArcaneEnlightenment where
  getModifiersFor _ (InvestigatorTarget iid) (ArcaneEnlightenment attrs) =
    pure [ toModifier attrs (HandSize 1) | ownedBy attrs iid ]
  getModifiersFor _ _ _ = pure []

instance HasActions env ArcaneEnlightenment where
  getActions i window (ArcaneEnlightenment x) = getActions i window x

slot :: AssetAttrs -> Slot
slot attrs = TraitRestrictedSlot (toSource attrs) Tome Nothing

instance (AssetRunner env) => RunMessage env ArcaneEnlightenment where
  runMessage msg (ArcaneEnlightenment attrs) = case msg of
    InvestigatorPlayAsset iid aid _ _ | aid == assetId attrs -> do
      unshiftMessage (AddSlot iid HandSlot (slot attrs))
      ArcaneEnlightenment <$> runMessage msg attrs
    _ -> ArcaneEnlightenment <$> runMessage msg attrs
