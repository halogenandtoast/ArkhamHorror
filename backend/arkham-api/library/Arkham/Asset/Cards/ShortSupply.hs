module Arkham.Asset.Cards.ShortSupply (shortSupply, ShortSupply (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher

newtype ShortSupply = ShortSupply AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shortSupply :: AssetCard ShortSupply
shortSupply = asset ShortSupply Cards.shortSupply

instance HasAbilities ShortSupply where
  getAbilities (ShortSupply attrs) =
    [ playerLimit PerGame $ restrictedAbility attrs 1 ControlsThis $ forced $ TurnBegins #when You
    ]

instance RunMessage ShortSupply where
  runMessage msg a@(ShortSupply attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ DiscardTopOfDeck iid 10 (toSource attrs) Nothing
      pure a
    _ -> ShortSupply <$> liftRunMessage msg attrs
