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
import Arkham.Types.LocationMatcher
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Window

newtype KeyToTheChamber = KeyToTheChamber AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

keyToTheChamber :: AssetCard KeyToTheChamber
keyToTheChamber = assetWith KeyToTheChamber Cards.keyToTheChamber (isStoryL .~ True)

instance (HasId LocationId env InvestigatorId, HasSet ConnectedLocationId env LocationId, HasId (Maybe LocationId) env LocationMatcher) => HasActions env KeyToTheChamber where
  getActions iid FastPlayerWindow (KeyToTheChamber attrs) | ownedBy attrs iid =
    do
      mHiddenChamberId <- getId @(Maybe LocationId)
        (LocationWithTitle "The Hidden Chamber")
      case mHiddenChamberId of
        Just hiddenChamberId -> do
          lid <- getId @LocationId iid
          connectedLocationIds <- map unConnectedLocationId <$> getSetList lid
          pure
            [ ActivateCardAbilityAction
                iid
                (mkAbility (toSource attrs) 1 (FastAbility Free))
            | hiddenChamberId `elem` connectedLocationIds
            ]
        Nothing -> pure []
  getActions iid window (KeyToTheChamber attrs) = getActions iid window attrs

instance HasModifiersFor env KeyToTheChamber where
  getModifiersFor = noModifiersFor

instance (HasQueue env, HasModifiersFor env (), HasId (Maybe LocationId) env LocationMatcher) => RunMessage env KeyToTheChamber where
  runMessage msg a@(KeyToTheChamber attrs) = case msg of
    Revelation iid source | isSource attrs source ->
      a <$ unshiftMessage (TakeControlOfAsset iid $ toId a)
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      mHiddenChamberId <- getId (LocationWithTitle "The Hidden Chamber")
      case mHiddenChamberId of
        Nothing -> throwIO $ InvalidState "The Hidden Chamber is missing"
        Just hiddenChamberId ->
          a <$ unshiftMessage
            (AttachAsset (toId a) (LocationTarget hiddenChamberId))
    _ -> KeyToTheChamber <$> runMessage msg attrs
