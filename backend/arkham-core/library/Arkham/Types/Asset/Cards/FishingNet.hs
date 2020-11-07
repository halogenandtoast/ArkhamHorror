{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.FishingNet
  ( FishingNet(..)
  , fishingNet
  )
where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Keyword

newtype FishingNet = FishingNet Attrs
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

fishingNet :: AssetId -> FishingNet
fishingNet uuid = FishingNet $ baseAttrs uuid "81021" $ pure ()

-- TODO: Removal of retaliate should maybe be a modifier of some sort
instance HasModifiersFor env FishingNet where
  getModifiersFor _ _ _ = pure []

ability :: Attrs -> Ability
ability attrs = mkAbility (toSource attrs) 1 (FastAbility FastPlayerWindow)

instance ActionRunner env => HasActions env FishingNet where
  getActions iid FastPlayerWindow (FishingNet attrs) | ownedBy attrs iid = do
    mrougarou <- asks (fmap unStoryEnemyId <$> getId (CardCode "81028"))
    case mrougarou of
      Nothing -> pure []
      Just eid -> do
        investigatorLocation <- asks $ getId @LocationId iid
        exhaustedEnemies <-
          asks
          $ map unExhaustedEnemyId
          . setToList
          . getSet investigatorLocation
        pure
          [ ActivateCardAbilityAction iid (ability attrs)
          | eid `elem` exhaustedEnemies
          ]
  getActions iid window (FishingNet x) = getActions iid window x

instance AssetRunner env => RunMessage env FishingNet where
  runMessage msg a@(FishingNet attrs@Attrs {..}) = case msg of
    UseCardAbility _ source _ 1 | isSource attrs source -> do
      mrougarou <- asks (fmap unStoryEnemyId <$> getId (CardCode "81028"))
      case mrougarou of
        Nothing -> error "can not use this ability"
        Just eid -> a <$ unshiftMessage (AttachAsset assetId (EnemyTarget eid))
    AttachAsset aid target | aid == assetId -> do
      unshiftMessage $ RemoveKeywords target [Retaliate]
      FishingNet <$> runMessage msg attrs
    _ -> FishingNet <$> runMessage msg attrs
