{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.CatBurgler1
  ( CatBurgler1(..)
  , catBurgler1
  )
where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner

newtype CatBurgler1 = CatBurgler1 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

catBurgler1 :: AssetId -> CatBurgler1
catBurgler1 uuid = CatBurgler1 $ baseAttrs uuid "01055" $ do
  slots .= [AllySlot]
  health ?= 2
  sanity ?= 2

instance HasModifiersFor env CatBurgler1 where
  getModifiersFor _ (InvestigatorTarget iid) (CatBurgler1 a) =
    pure [ SkillModifier SkillAgility 1 | ownedBy a iid ]
  getModifiersFor _ _ _ = pure []

ability :: Attrs -> Ability
ability attrs = mkAbility (toSource attrs) 1 (ActionAbility 1 Nothing)

instance ActionRunner env => HasActions env CatBurgler1 where
  getActions iid NonFast (CatBurgler1 a) | ownedBy a iid = do
    hasActionsRemaining <- getHasActionsRemaining iid Nothing mempty
    pure
      [ ActivateCardAbilityAction iid (ability a)
      | not (assetExhausted a) && hasActionsRemaining
      ]
  getActions i window (CatBurgler1 x) = getActions i window x

instance (AssetRunner env) => RunMessage env CatBurgler1 where
  runMessage msg (CatBurgler1 attrs) = case msg of
    InvestigatorPlayAsset iid aid _ _ | aid == assetId attrs -> do
      unshiftMessage $ AddModifiers
        (InvestigatorTarget iid)
        (toSource attrs)
        [SkillModifier SkillAgility 1]
      CatBurgler1 <$> runMessage msg attrs
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      engagedEnemyIds <- asks $ setToList . getSet iid
      locationId <- asks $ getId @LocationId iid
      accessibleLocationIds <-
        asks $ map unAccessibleLocationId . setToList . getSet locationId
      unshiftMessages
        $ [ DisengageEnemy iid eid | eid <- engagedEnemyIds ]
        <> [ chooseOne
               iid
               [ MoveAction iid lid False | lid <- accessibleLocationIds ]
           | not (null accessibleLocationIds)
           ]
      pure $ CatBurgler1 $ attrs & exhausted .~ True
    _ -> CatBurgler1 <$> runMessage msg attrs
