module Arkham.Types.Asset.Cards.LitaChantler where

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

newtype LitaChantler = LitaChantler AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

litaChantler :: AssetId -> LitaChantler
litaChantler uuid = LitaChantler $ (baseAttrs uuid "01117")
  { assetSlots = [AllySlot]
  , assetHealth = Just 3
  , assetSanity = Just 3
  }

instance HasId LocationId env InvestigatorId => HasModifiersFor env LitaChantler where
  getModifiersFor _ (InvestigatorTarget iid) (LitaChantler a@AssetAttrs {..}) = do
    locationId <- getId @LocationId iid
    case assetInvestigator of
      Nothing -> pure []
      Just ownerId -> do
        sameLocation <- (== locationId) <$> getId ownerId
        pure [ toModifier a (SkillModifier SkillCombat 1) | sameLocation ]
  getModifiersFor _ _ _ = pure []

ability :: EnemyId -> AssetAttrs -> Ability
ability eid a = (mkAbility (toSource a) 1 (ReactionAbility Free))
  { abilityMetadata = Just $ TargetMetadata (EnemyTarget eid)
  }

instance HasSet Trait env EnemyId => HasActions env LitaChantler where
  getActions i (WhenSuccessfulAttackEnemy who eid) (LitaChantler a)
    | ownedBy a i && who `elem` [You, InvestigatorAtYourLocation] = do
      traits <- getSetList eid
      pure
        [ ActivateCardAbilityAction i (ability eid a) | Monster `elem` traits ]
  getActions i window (LitaChantler a) = getActions i window a

instance (AssetRunner env) => RunMessage env LitaChantler where
  runMessage msg a@(LitaChantler attrs@AssetAttrs {..}) = case msg of
    UseCardAbility _ source (Just (TargetMetadata target)) 1 _
      | isSource attrs source -> do
        a <$ unshiftMessage
          (CreateWindowModifierEffect
            EffectSkillTestWindow
            (EffectModifiers [toModifier attrs (DamageTaken 1)])
            source
            target
          )
    _ -> LitaChantler <$> runMessage msg attrs

