module Arkham.Types.Asset.Cards.StrayCat
  ( StrayCat(..)
  , strayCat
  ) where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner

newtype StrayCat = StrayCat AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strayCat :: AssetId -> StrayCat
strayCat uuid = StrayCat
  $ (baseAttrs uuid "01076") { assetSlots = [AllySlot], assetHealth = Just 2 }

instance HasModifiersFor env StrayCat where
  getModifiersFor = noModifiersFor

ability :: AssetAttrs -> Ability
ability a = mkAbility (toSource a) 1 (FastAbility (DiscardCost $ toTarget a))

instance HasActions env StrayCat where
  getActions iid FastPlayerWindow (StrayCat a) | ownedBy a iid =
    withBaseActions iid FastPlayerWindow a
      $ pure [ActivateCardAbilityAction iid (ability a)]
  getActions _ _ _ = pure []

-- | See: PlayerCardWithBehavior
instance AssetRunner env => RunMessage env StrayCat where
  runMessage msg (StrayCat attrs) = StrayCat <$> runMessage msg attrs
