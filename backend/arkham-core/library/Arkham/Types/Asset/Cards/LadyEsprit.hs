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
ladyEsprit uuid = LadyEsprit $ (baseAttrs uuid "81019")
  { assetSlots = [AllySlot]
  , assetHealth = Just 2
  , assetSanity = Just 4
  }

ability :: Attrs -> Ability
ability attrs = mkAbility (toSource attrs) 1 (ActionAbility 1 Nothing)

instance HasModifiersFor env LadyEsprit where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env LadyEsprit where
  getActions iid NonFast (LadyEsprit a@Attrs {..}) = do
    canAffordActions <- getCanAffordCost
      iid
      (toSource a)
      (ActionCost 1 Nothing assetTraits)
    locationId <- getId @LocationId iid
    assetLocationId <- case assetInvestigator of
      Nothing -> pure $ fromJustNote "must be set" assetLocation
      Just iid' -> getId iid'
    pure
      [ ActivateCardAbilityAction iid (ability a)
      | not assetExhausted && locationId == assetLocationId && canAffordActions
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
        (LadyEsprit $ attrs & exhaustedL .~ True & sanityDamageL +~ 1)
    _ -> LadyEsprit <$> runMessage msg attrs
