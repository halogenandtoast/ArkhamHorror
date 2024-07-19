module Arkham.Asset.Cards.StylishCoat1 (stylishCoat1, StylishCoat1 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Modifier

newtype StylishCoat1 = StylishCoat1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stylishCoat1 :: AssetCard StylishCoat1
stylishCoat1 = assetWith StylishCoat1 Cards.stylishCoat1 $ (healthL ?~ 1) . (sanityL ?~ 1)

instance HasAbilities StylishCoat1 where
  getAbilities (StylishCoat1 a) =
    [ controlledAbility a 1 (DuringTurn You)
        $ ReactionAbility
          (GainsResources #when You (SourceIsPlayerCard <> SourceIsCardEffect) (atLeast 1))
          (exhaust a)
    ]

instance RunMessage StylishCoat1 where
  runMessage msg a@(StylishCoat1 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gainResourcesModifier iid (attrs.ability 1) iid (AdditionalResources 1)
      pure a
    _ -> StylishCoat1 <$> liftRunMessage msg attrs
