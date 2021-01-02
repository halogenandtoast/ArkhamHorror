{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.StrayCat where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner

newtype StrayCat = StrayCat Attrs
  deriving newtype (Show, ToJSON, FromJSON)

strayCat :: AssetId -> StrayCat
strayCat uuid = StrayCat
  $ (baseAttrs uuid "01076") { assetSlots = [AllySlot], assetHealth = Just 2 }

instance HasModifiersFor env StrayCat where
  getModifiersFor = noModifiersFor

instance HasActions env StrayCat where
  getActions iid window (StrayCat a) | ownedBy a iid =
    withBaseActions iid window a $ do
      let
        ability = mkAbility
          (toSource a)
          1
          (FastAbility window (DiscardCost $ toTarget a))
      pure [ActivateCardAbilityAction iid ability]
  getActions _ _ _ = pure []

instance AssetRunner env => RunMessage env StrayCat where
  runMessage msg (StrayCat attrs) = StrayCat <$> runMessage msg attrs
