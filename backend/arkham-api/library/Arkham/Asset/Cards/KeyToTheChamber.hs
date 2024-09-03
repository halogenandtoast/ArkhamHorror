module Arkham.Asset.Cards.KeyToTheChamber (
  keyToTheChamber,
  KeyToTheChamber (..),
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Exception
import Arkham.Matcher
import Arkham.Placement
import Arkham.Prelude

newtype KeyToTheChamber = KeyToTheChamber AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

keyToTheChamber :: AssetCard KeyToTheChamber
keyToTheChamber =
  assetWith KeyToTheChamber Cards.keyToTheChamber (isStoryL .~ True)

instance HasAbilities KeyToTheChamber where
  getAbilities (KeyToTheChamber attrs) = case attrs.placement of
    InPlayArea _ ->
      [ controlledAbility attrs 1 (exists (ConnectedLocation <> "The Hidden Chamber")) (FastAbility Free)
      ]
    _ -> []

instance RunMessage KeyToTheChamber where
  runMessage msg a@(KeyToTheChamber attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      a <$ push (TakeControlOfAsset iid $ toId a)
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      mHiddenChamberId <- selectOne (LocationWithTitle "The Hidden Chamber")
      case mHiddenChamberId of
        Nothing -> throwIO $ InvalidState "The Hidden Chamber is missing"
        Just hiddenChamberId -> push (AttachAsset (toId a) (LocationTarget hiddenChamberId))
      pure a
    _ -> KeyToTheChamber <$> runMessage msg attrs
