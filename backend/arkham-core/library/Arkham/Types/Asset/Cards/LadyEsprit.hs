{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.LadyEsprit
  ( LadyEsprit(..)
  , ladyEsprit
  )
where

import Arkham.Import

import Arkham.Types.Asset.Attrs
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

instance HasModifiersFor env investigator LadyEsprit where
  getModifiersFor _ _ _ = pure []

instance ActionRunner env  investigator => HasActions env investigator LadyEsprit where
  getActions i NonFast (LadyEsprit a@Attrs {..}) = do
    locationId <- case assetInvestigator of
      Nothing -> pure $ fromJustNote "must be set" assetLocation
      Just iid -> asks (getId iid)
    pure
      [ ActivateCardAbilityAction (getId () i) (ability a)
      | not assetExhausted && locationOf i == locationId
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
