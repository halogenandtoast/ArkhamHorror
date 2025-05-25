module Arkham.Asset.Assets.BookOfShadows1 (bookOfShadows1) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype BookOfShadows1 = BookOfShadows1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bookOfShadows1 :: AssetCard BookOfShadows1
bookOfShadows1 = asset BookOfShadows1 Cards.bookOfShadows1

instance HasAbilities BookOfShadows1 where
  getAbilities (BookOfShadows1 a) =
    [ controlled a 1 (exists (AssetControlledBy You <> #spell))
        $ actionAbilityWithCost (ResourceCost 1 <> exhaust a)
    ]

instance RunMessage BookOfShadows1 where
  runMessage msg a@(BookOfShadows1 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assets <- select $ assetControlledBy iid <> #spell
      chooseTargetM iid assets (addUsesOn (attrs.ability 1) #charge 1)
      pure a
    _ -> BookOfShadows1 <$> liftRunMessage msg attrs
