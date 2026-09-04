{- | The runner behind a debug-authored custom asset. See "Arkham.Custom.Enemy".

Health and sanity are not 'CardDef' fields, so they come from the def's meta
(@health@/@sanity@), which is where the debug editor puts them.
-}
module Arkham.Custom.Asset (CustomAsset (..), customAsset) where

import Arkham.Asset.Import.Lifted
import Arkham.Card.CardDef (CardDef)
import Arkham.Card.CustomCard (customMeta)

newtype CustomAsset = CustomAsset AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

customAsset :: CardDef -> AssetCard CustomAsset
customAsset def =
  ally CustomAsset def (customMeta "health" 0 def, customMeta "sanity" 0 def)

instance RunMessage CustomAsset where
  runMessage msg (CustomAsset attrs) = CustomAsset <$> runMessage msg attrs
