module Arkham.Types.Asset.Cards.ArcaneInitiate where

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

newtype ArcaneInitiate = ArcaneInitiate AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arcaneInitiate :: AssetId -> ArcaneInitiate
arcaneInitiate uuid = ArcaneInitiate $ (baseAttrs uuid "01063")
  { assetSlots = [AllySlot]
  , assetHealth = Just 1
  , assetSanity = Just 2
  }

fastAbility :: AssetAttrs -> Ability
fastAbility a =
  mkAbility (toSource a) 1 (FastAbility $ ExhaustCost (toTarget a))

instance HasModifiersFor env ArcaneInitiate where
  getModifiersFor = noModifiersFor

instance HasActions env ArcaneInitiate where
  getActions iid FastPlayerWindow (ArcaneInitiate a) | ownedBy a iid =
    pure [ActivateCardAbilityAction iid $ fastAbility a]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env ArcaneInitiate where
  runMessage msg a@(ArcaneInitiate attrs) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId attrs ->
      ArcaneInitiate <$> runMessage msg (attrs & doomL +~ 1)
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ unshiftMessage
      (chooseOne
        iid
        [SearchTopOfDeck iid (InvestigatorTarget iid) 3 [Spell] ShuffleBackIn]
      )
    _ -> ArcaneInitiate <$> runMessage msg attrs
