module Arkham.Types.Asset.Cards.ForbiddenKnowledge where

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

newtype ForbiddenKnowledge = ForbiddenKnowledge AssetAttrs
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

forbiddenKnowledge :: AssetId -> ForbiddenKnowledge
forbiddenKnowledge uuid = ForbiddenKnowledge $ baseAttrs uuid "01058"

instance HasModifiersFor env ForbiddenKnowledge where
  getModifiersFor = noModifiersFor

instance HasActions env ForbiddenKnowledge where
  getActions iid FastPlayerWindow (ForbiddenKnowledge a) | ownedBy a iid = pure
    [ ActivateCardAbilityAction
        iid
        (mkAbility
          (toSource a)
          1
          (FastAbility $ Costs
            [ UseCost (toId a) Secret 1
            , HorrorCost (toSource a) (InvestigatorTarget iid) 1
            ]
          )
        )
    | useCount (assetUses a) > 0
    ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env ForbiddenKnowledge where
  runMessage msg a@(ForbiddenKnowledge attrs) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId attrs ->
      ForbiddenKnowledge <$> runMessage msg (attrs & usesL .~ Uses Secret 4)
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ unshiftMessage (TakeResources iid 1 False)
    _ -> ForbiddenKnowledge <$> runMessage msg attrs
