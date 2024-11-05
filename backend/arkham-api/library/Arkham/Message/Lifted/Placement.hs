module Arkham.Message.Lifted.Placement where

import Arkham.Asset.Types
import Arkham.Classes.HasQueue
import Arkham.Enemy.Types
import Arkham.Event.Types
import Arkham.Id
import Arkham.Message
import Arkham.Message.Lifted.Queue
import Arkham.Placement
import Arkham.Prelude
import Arkham.Treachery.Types

class IsPlacement a where
  toPlacement :: a -> Placement

class Placeable a where
  place :: (ReverseQueue m, IsPlacement p) => a -> p -> m ()

instance IsPlacement Placement where
  toPlacement = id

instance IsPlacement LocationId where
  toPlacement = AtLocation

instance IsPlacement AssetId where
  toPlacement = (`AttachedToAsset` Nothing)

instance Placeable TreacheryAttrs where
  place attrs placement = push $ PlaceTreachery attrs.id (toPlacement placement)

instance Placeable EventAttrs where
  place attrs placement = push $ PlaceEvent attrs.id (toPlacement placement)

instance Placeable AssetAttrs where
  place attrs placement = push $ PlaceAsset attrs.id (toPlacement placement)

instance Placeable AssetId where
  place aid placement = push $ PlaceAsset aid (toPlacement placement)

instance Placeable EnemyId where
  place aid placement = push $ PlaceEnemy aid (toPlacement placement)

instance Placeable EnemyAttrs where
  place attrs placement = push $ PlaceEnemy attrs.id (toPlacement placement)

instance Placeable InvestigatorId where
  place iid placement = push $ PlaceInvestigator iid (toPlacement placement)
