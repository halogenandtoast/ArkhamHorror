module Arkham.Types.Asset.Cards.FirstAid
  ( FirstAid(..)
  , firstAid
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
import Arkham.Types.Asset.Uses

newtype FirstAid = FirstAid AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

firstAid :: AssetId -> FirstAid
firstAid uuid = FirstAid $ baseAttrs uuid "01019"

instance HasModifiersFor env FirstAid where
  getModifiersFor = noModifiersFor

ability :: AssetAttrs -> Ability
ability attrs = mkAbility
  (toSource attrs)
  1
  (ActionAbility Nothing $ Costs [ActionCost 1, UseCost (toId attrs) Supply 1])

instance HasActions env FirstAid where
  getActions iid NonFast (FirstAid a) =
    pure [ ActivateCardAbilityAction iid (ability a) | ownedBy a iid ]
  getActions _ _ _ = pure []

instance AssetRunner env => RunMessage env FirstAid where
  runMessage msg a@(FirstAid attrs@AssetAttrs {..}) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId ->
      FirstAid <$> runMessage msg (attrs & usesL .~ Uses Supply 3)
    UseCardAbility iid (AssetSource aid) _ 1 _ | aid == assetId -> do
      lid <- getId @LocationId iid
      investigatorTargets <- map InvestigatorTarget <$> getSetList lid
      a <$ unshiftMessage
        (chooseOne
          iid
          [ TargetLabel
              target
              [chooseOne iid [HealDamage target 1, HealHorror target 1]]
          | target <- investigatorTargets
          ]
        )
    _ -> FirstAid <$> runMessage msg attrs
