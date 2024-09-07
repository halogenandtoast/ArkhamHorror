module Arkham.Message.Lifted.Placement where

import Arkham.Asset.Types
import Arkham.Classes.HasQueue
import Arkham.Event.Types
import Arkham.Id
import Arkham.Message
import Arkham.Message.Lifted.Queue
import Arkham.Placement
import Arkham.Prelude

class IsPlacement a where
  toPlacement :: a -> Placement

class Placeable a where
  place :: (ReverseQueue m, IsPlacement p) => a -> p -> m ()

instance IsPlacement Placement where
  toPlacement = id

instance IsPlacement AssetId where
  toPlacement = (`AttachedToAsset` Nothing)

instance Placeable EventAttrs where
  place attrs placement = push $ PlaceEvent attrs.id (toPlacement placement)

instance Placeable AssetAttrs where
  place attrs placement = push $ PlaceAsset attrs.id (toPlacement placement)
