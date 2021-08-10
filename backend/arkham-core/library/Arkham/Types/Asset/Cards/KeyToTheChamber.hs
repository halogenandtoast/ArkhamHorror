module Arkham.Types.Asset.Cards.KeyToTheChamber
  ( keyToTheChamber
  , KeyToTheChamber(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Exception
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Restriction
import Arkham.Types.Target

newtype KeyToTheChamber = KeyToTheChamber AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

keyToTheChamber :: AssetCard KeyToTheChamber
keyToTheChamber =
  assetWith KeyToTheChamber Cards.keyToTheChamber (isStoryL .~ True)

instance HasActions KeyToTheChamber where
  getActions (KeyToTheChamber attrs) =
    [ restrictedAbility
        attrs
        1
        (OwnsThis <> LocationExists
          (ConnectedLocation <> LocationWithTitle "The Hidden Chamber")
        )
        (FastAbility Free)
    ]

instance HasModifiersFor env KeyToTheChamber

instance (HasQueue env, HasModifiersFor env (), HasId (Maybe LocationId) env LocationMatcher) => RunMessage env KeyToTheChamber where
  runMessage msg a@(KeyToTheChamber attrs) = case msg of
    Revelation iid source | isSource attrs source ->
      a <$ push (TakeControlOfAsset iid $ toId a)
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      mHiddenChamberId <- getId (LocationWithTitle "The Hidden Chamber")
      case mHiddenChamberId of
        Nothing -> throwIO $ InvalidState "The Hidden Chamber is missing"
        Just hiddenChamberId ->
          a <$ push (AttachAsset (toId a) (LocationTarget hiddenChamberId))
    _ -> KeyToTheChamber <$> runMessage msg attrs
