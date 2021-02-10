module Arkham.Types.Asset.Cards.HeirloomOfHyperborea where

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
import Arkham.Types.Asset.Runner
import Arkham.Types.Trait

newtype HeirloomOfHyperborea = HeirloomOfHyperborea AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

heirloomOfHyperborea :: AssetId -> HeirloomOfHyperborea
heirloomOfHyperborea uuid = HeirloomOfHyperborea
  $ (baseAttrs uuid "01012") { assetSlots = [AccessorySlot] }

instance HasModifiersFor env HeirloomOfHyperborea where
  getModifiersFor = noModifiersFor

reactionAbility :: AssetAttrs -> Ability
reactionAbility attrs = mkAbility (toSource attrs) 1 (FastAbility Free)

instance HasActions env HeirloomOfHyperborea where
  getActions iid (AfterPlayCard You traits) (HeirloomOfHyperborea a)
    | ownedBy a iid
    = pure
      [ ActivateCardAbilityAction iid (reactionAbility a)
      | Spell `elem` traits
      ]
  getActions i window (HeirloomOfHyperborea x) = getActions i window x

instance (AssetRunner env) => RunMessage env HeirloomOfHyperborea where
  runMessage msg a@(HeirloomOfHyperborea attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ unshiftMessage (DrawCards iid 1 False)
    _ -> HeirloomOfHyperborea <$> runMessage msg attrs
