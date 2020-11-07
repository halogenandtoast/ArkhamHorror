{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.LadyEsprit
  ( LadyEsprit(..)
  , ladyEsprit
  )
where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner

newtype LadyEsprit = LadyEsprit Attrs
  deriving newtype (Show, ToJSON, FromJSON)

ladyEsprit :: AssetId -> LadyEsprit
ladyEsprit uuid = LadyEsprit $ baseAttrs uuid "81019" $ do
  slots .= [AllySlot]
  health ?= 2
  sanity ?= 4

ability :: Attrs -> Ability
ability attrs = mkAbility (toSource attrs) 1 (ActionAbility 1 Nothing)

instance HasModifiersFor env LadyEsprit where
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env LadyEsprit where
  getActions iid NonFast (LadyEsprit a@Attrs {..}) = do
    hasActionsRemaining <- getHasActionsRemaining iid Nothing mempty
    locationId <- asks $ getId @LocationId iid
    assetLocationId <- case assetInvestigator of
      Nothing -> pure $ fromJustNote "must be set" assetLocation
      Just iid' -> asks (getId iid')
    pure
      [ ActivateCardAbilityAction iid (ability a)
      | not assetExhausted
        && locationId
        == assetLocationId
        && hasActionsRemaining
      ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env LadyEsprit where
  runMessage msg (LadyEsprit attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      unshiftMessage $ chooseOne
        iid
        [HealDamage (InvestigatorTarget iid) 2, TakeResources iid 2 False]
      runMessage
        CheckDefeated
        (LadyEsprit $ attrs & exhausted .~ True & sanityDamage +~ 1)
    _ -> LadyEsprit <$> runMessage msg attrs
