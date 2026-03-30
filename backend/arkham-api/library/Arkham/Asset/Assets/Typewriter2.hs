module Arkham.Asset.Assets.Typewriter2 (typewriter2) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Trait (Trait (Tome))

newtype Typewriter2 = Typewriter2 AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

typewriter2 :: AssetCard Typewriter2
typewriter2 = assetWith Typewriter2 Cards.typewriter2 discardWhenNoUses

instance HasModifiersFor Typewriter2 where
  getModifiersFor (Typewriter2 a) = for_ a.controller \iid ->
    modifySelectWhen
      a
      (a.use Secret > 0)
      (withTrait Tome <> assetControlledBy iid)
      [ProvidesUses Secret (toSource a)]

instance RunMessage Typewriter2 where
  runMessage msg (Typewriter2 attrs) = Typewriter2 <$> runMessage msg attrs
