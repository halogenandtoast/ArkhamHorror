module Arkham.Types.Asset.Cards.WendysAmulet where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Modifier
import Arkham.Types.Target

newtype WendysAmulet = WendysAmulet AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wendysAmulet :: AssetCard WendysAmulet
wendysAmulet = accessory WendysAmulet Cards.wendysAmulet

instance HasModifiersFor env WendysAmulet where
  getModifiersFor _ (InvestigatorTarget iid) (WendysAmulet a) =
    pure $ toModifiers
      a
      [ CanPlayTopOfDiscard (Just EventType, []) | ownedBy a iid ]
  getModifiersFor _ _ _ = pure []

instance HasActions env WendysAmulet where
  getActions i window (WendysAmulet x) = getActions i window x

instance (AssetRunner env) => RunMessage env WendysAmulet where
  runMessage msg (WendysAmulet attrs) = WendysAmulet <$> runMessage msg attrs
