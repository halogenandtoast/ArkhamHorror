{- | The runner behind a debug-authored custom asset. See "Arkham.Custom.Enemy".

Health and sanity are not 'CardDef' fields, so they come from the def's meta
(@health@/@sanity@), which is where the debug editor puts them.
-}
module Arkham.Custom.Asset (CustomAsset (..), customAsset) where

import Arkham.Asset.Import.Lifted
import Arkham.Card.CardDef (CardDef)
import Arkham.Card.CustomCard (customMeta)
import Arkham.Custom.Ability (customAbilities, customModifiers, runCustomAbility, runCustomHandlers)

newtype CustomAsset = CustomAsset AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

customAsset :: CardDef -> AssetCard CustomAsset
customAsset def =
  ally CustomAsset def (customMeta "health" 0 def, customMeta "sanity" 0 def)

instance HasModifiersFor CustomAsset where
  getModifiersFor (CustomAsset a) = customModifiers a

instance HasAbilities CustomAsset where
  getAbilities (CustomAsset a) = customAbilities a

instance RunMessage CustomAsset where
  runMessage msg x@(CustomAsset attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) idx -> do
      runCustomAbility attrs iid idx
      pure x
    _ -> do
      runCustomHandlers attrs msg
      CustomAsset <$> liftRunMessage msg attrs
