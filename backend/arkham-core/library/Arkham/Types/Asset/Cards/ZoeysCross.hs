module Arkham.Types.Asset.Cards.ZoeysCross
  ( ZoeysCross(..)
  , zoeysCross
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


import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype ZoeysCross = ZoeysCross AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

zoeysCross :: AssetId -> ZoeysCross
zoeysCross uuid =
  ZoeysCross $ (baseAttrs uuid "02006") { assetSlots = [AccessorySlot] }

instance HasModifiersFor env ZoeysCross where
  getModifiersFor = noModifiersFor

ability :: AssetAttrs -> EnemyId -> Ability
ability attrs eid = base
  { abilityMetadata = Just (TargetMetadata (EnemyTarget eid))
  }
 where
  base = mkAbility
    (toSource attrs)
    1
    (ReactionAbility $ Costs [ExhaustCost (toTarget attrs), ResourceCost 1])

instance HasActions env ZoeysCross where
  getActions iid (AfterEnemyEngageInvestigator You eid) (ZoeysCross a@AssetAttrs {..})
    | ownedBy a iid
    = pure [ActivateCardAbilityAction iid (ability a eid)]
  getActions i window (ZoeysCross x) = getActions i window x

instance (AssetRunner env) => RunMessage env ZoeysCross where
  runMessage msg a@(ZoeysCross attrs) = case msg of
    UseCardAbility iid source (Just (TargetMetadata (EnemyTarget eid))) 1 _
      | isSource attrs source -> a
      <$ unshiftMessage (EnemyDamage eid iid source 1)
    _ -> ZoeysCross <$> runMessage msg attrs
