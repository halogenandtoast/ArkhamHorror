module Arkham.Asset.Cards.LoneWolf (loneWolf, LoneWolf (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher

newtype LoneWolf = LoneWolf AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

loneWolf :: AssetCard LoneWolf
loneWolf = asset LoneWolf Cards.loneWolf

instance HasAbilities LoneWolf where
  getAbilities (LoneWolf x) = [controlledAbility x 1 InvestigatorIsAlone $ freeReaction $ TurnBegins #when You]

instance RunMessage LoneWolf where
  runMessage msg a@(LoneWolf attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gainResourcesIfCan iid (attrs.ability 1) 1
      pure a
    _ -> LoneWolf <$> liftRunMessage msg attrs
