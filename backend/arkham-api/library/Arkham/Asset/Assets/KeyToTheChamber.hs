module Arkham.Asset.Assets.KeyToTheChamber (keyToTheChamber) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Placement

newtype KeyToTheChamber = KeyToTheChamber AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

keyToTheChamber :: AssetCard KeyToTheChamber
keyToTheChamber = asset KeyToTheChamber Cards.keyToTheChamber

instance HasAbilities KeyToTheChamber where
  getAbilities (KeyToTheChamber attrs) = case attrs.placement of
    InPlayArea _ ->
      [ controlled attrs 1 (exists (ConnectedLocation <> "The Hidden Chamber")) (FastAbility Free)
      ]
    _ -> []

instance RunMessage KeyToTheChamber where
  runMessage msg a@(KeyToTheChamber attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      takeControlOfAsset iid attrs
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectForMaybeM (LocationWithTitle "The Hidden Chamber") (attach attrs)
      pure a
    _ -> KeyToTheChamber <$> liftRunMessage msg attrs
