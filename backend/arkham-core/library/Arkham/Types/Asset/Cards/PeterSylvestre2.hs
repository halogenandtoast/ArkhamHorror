module Arkham.Types.Asset.Cards.PeterSylvestre2
  ( PeterSylvestre2(..)
  , peterSylvestre2
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
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner

newtype PeterSylvestre2 = PeterSylvestre2 AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

peterSylvestre2 :: AssetId -> PeterSylvestre2
peterSylvestre2 uuid = PeterSylvestre2 $ (baseAttrs uuid "02035")
  { assetSlots = [AllySlot]
  , assetHealth = Just 1
  , assetSanity = Just 3
  }

instance HasModifiersFor env PeterSylvestre2 where
  getModifiersFor _ (InvestigatorTarget iid) (PeterSylvestre2 a)
    | ownedBy a iid = pure $ toModifiers
      a
      [SkillModifier SkillAgility 1, SkillModifier SkillWillpower 1]
  getModifiersFor _ _ _ = pure []

ability :: AssetAttrs -> Ability
ability attrs = mkAbility (toSource attrs) 1 (ReactionAbility Free)

instance HasActions env PeterSylvestre2 where
  getActions iid (AfterEndTurn You) (PeterSylvestre2 a) | ownedBy a iid =
    pure [ ActivateCardAbilityAction iid (ability a) | assetSanityDamage a > 0 ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env PeterSylvestre2 where
  runMessage msg (PeterSylvestre2 attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      pure $ PeterSylvestre2 $ attrs & sanityDamageL -~ 1
    _ -> PeterSylvestre2 <$> runMessage msg attrs
