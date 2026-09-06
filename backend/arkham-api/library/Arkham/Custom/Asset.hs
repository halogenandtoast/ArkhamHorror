{- | The runner behind a debug-authored custom asset. See "Arkham.Custom.Enemy".

Health and sanity are not 'CardDef' fields, so they come from the def's meta
(@health@/@sanity@), which is where the debug editor puts them.
-}
module Arkham.Custom.Asset (CustomAsset (..), customAsset) where

import Arkham.Asset.Import.Lifted
import Arkham.Card.CardDef (CardDef)
import Arkham.Card.CustomCard (customMeta)
import Arkham.Custom.Ability (
  customAbilities,
  customModifiers,
  isCustomAbility,
  runCustomAbility,
  runCustomHandlers,
  runCustomRevelation,
  pattern ZonedUseThisAbility,
 )

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
    ZonedUseThisAbility iid (isSource attrs -> True) idx ws | isCustomAbility attrs idx -> do
      runCustomAbility attrs iid idx ws
      pure x
    -- What it does when it is revealed. Not an ability: no one activates it, and
    -- the card may have to place itself before the engine tidies it away.
    Revelation iid (isSource attrs -> True) -> do
      runCustomRevelation attrs iid
      runCustomHandlers attrs msg
      CustomAsset <$> liftRunMessage msg attrs
    _ -> do
      runCustomHandlers attrs msg
      CustomAsset <$> liftRunMessage msg attrs
