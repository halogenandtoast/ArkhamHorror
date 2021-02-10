module Arkham.Types.Asset.Cards.LeatherCoat where

import Arkham.Prelude

import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Slot
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype LeatherCoat = LeatherCoat AssetAttrs
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

leatherCoat :: AssetId -> LeatherCoat
leatherCoat uuid = LeatherCoat
  $ (baseAttrs uuid "01072") { assetSlots = [BodySlot], assetHealth = Just 2 }

instance HasModifiersFor env LeatherCoat where
  getModifiersFor = noModifiersFor

instance HasActions env LeatherCoat where
  getActions i window (LeatherCoat x) = getActions i window x

instance (AssetRunner env) => RunMessage env LeatherCoat where
  runMessage msg (LeatherCoat attrs) = LeatherCoat <$> runMessage msg attrs
