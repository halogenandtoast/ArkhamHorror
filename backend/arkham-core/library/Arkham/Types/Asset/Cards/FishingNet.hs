module Arkham.Types.Asset.Cards.FishingNet
  ( FishingNet(..)
  , fishingNet
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
import Arkham.Types.Asset.Runner
import Arkham.Types.Game.Helpers
import Arkham.Types.Keyword

newtype FishingNet = FishingNet AssetAttrs
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

fishingNet :: AssetId -> FishingNet
fishingNet uuid = FishingNet $ (baseAttrs uuid "81021") { assetIsStory = True }

instance HasModifiersFor env FishingNet where
  getModifiersFor _ (EnemyTarget eid) (FishingNet attrs) = pure $ toModifiers
    attrs
    [ RemoveKeyword Retaliate | assetEnemy attrs == Just eid ]
  getModifiersFor _ _ _ = pure []

ability :: AssetAttrs -> Ability
ability attrs = mkAbility (toSource attrs) 1 (FastAbility Free)

instance ActionRunner env => HasActions env FishingNet where
  getActions iid FastPlayerWindow (FishingNet attrs) | ownedBy attrs iid = do
    mrougarou <- fmap unStoryEnemyId <$> getId (CardCode "81028")
    case mrougarou of
      Nothing -> pure []
      Just eid -> do
        investigatorLocation <- getId @LocationId iid
        exhaustedEnemies <- map unExhaustedEnemyId
          <$> getSetList investigatorLocation
        pure
          [ ActivateCardAbilityAction iid (ability attrs)
          | eid `elem` exhaustedEnemies && isNothing (assetEnemy attrs)
          ]
  getActions iid window (FishingNet x) = getActions iid window x

instance AssetRunner env => RunMessage env FishingNet where
  runMessage msg a@(FishingNet attrs@AssetAttrs {..}) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      mrougarou <- fmap unStoryEnemyId <$> getId (CardCode "81028")
      case mrougarou of
        Nothing -> error "can not use this ability"
        Just eid -> a <$ unshiftMessage (AttachAsset assetId (EnemyTarget eid))
    _ -> FishingNet <$> runMessage msg attrs
