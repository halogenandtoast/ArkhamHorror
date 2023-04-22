module Arkham.Asset.Cards.KeyToTheChamber
  ( keyToTheChamber
  , KeyToTheChamber(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Exception
import Arkham.Matcher

newtype KeyToTheChamber = KeyToTheChamber AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

keyToTheChamber :: AssetCard KeyToTheChamber
keyToTheChamber =
  assetWith KeyToTheChamber Cards.keyToTheChamber (isStoryL .~ True)

instance HasAbilities KeyToTheChamber where
  getAbilities (KeyToTheChamber attrs) =
    [ restrictedAbility
        attrs
        1
        (ControlsThis <> LocationExists
          (ConnectedLocation <> LocationWithTitle "The Hidden Chamber")
        )
        (FastAbility Free)
    ]

instance HasModifiersFor KeyToTheChamber

instance RunMessage KeyToTheChamber where
  runMessage msg a@(KeyToTheChamber attrs) = case msg of
    Revelation iid source | isSource attrs source ->
      a <$ push (TakeControlOfAsset iid $ toId a)
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      mHiddenChamberId <- selectOne (LocationWithTitle "The Hidden Chamber")
      case mHiddenChamberId of
        Nothing -> throwIO $ InvalidState "The Hidden Chamber is missing"
        Just hiddenChamberId ->
          a <$ push (AttachAsset (toId a) (LocationTarget hiddenChamberId))
    _ -> KeyToTheChamber <$> runMessage msg attrs
