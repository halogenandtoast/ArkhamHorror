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
      -- N.B. we are special casing control here to avoid a weird interaction
      -- where the key would be discarded after being attached if the previous
      -- controller is eliminated as they technically retain control
      pure . KeyToTheChamber $ attrs & controllerL .~ Nothing
    _ -> KeyToTheChamber <$> liftRunMessage msg attrs
