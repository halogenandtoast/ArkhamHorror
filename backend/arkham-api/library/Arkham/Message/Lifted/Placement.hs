module Arkham.Message.Lifted.Placement (module Arkham.Message.Lifted.Placement, module Arkham.Placement) where

import Arkham.Asset.Types
import Arkham.Classes.HasQueue
import Arkham.Enemy.Types
import Arkham.Event.Types
import Arkham.Id
import Arkham.Message
import Arkham.Message.Lifted.Queue
import Arkham.Placement
import Arkham.Prelude
import Arkham.Skill.Types
import Arkham.Treachery.Types

class Placeable a where
  place :: (ReverseQueue m, IsPlacement p) => a -> p -> m ()

instance Placeable TreacheryAttrs where
  place attrs placement = push $ PlaceTreachery attrs.id (toPlacement placement)

instance Placeable EventAttrs where
  place attrs placement = push $ PlaceEvent attrs.id (toPlacement placement)

instance Placeable AssetAttrs where
  place attrs placement = push $ PlaceAsset attrs.id (toPlacement placement)

instance Placeable EventId where
  place eid placement = push $ PlaceEvent eid (toPlacement placement)

instance Placeable AssetId where
  place aid placement = push $ PlaceAsset aid (toPlacement placement)

instance Placeable EnemyId where
  place aid placement = push $ PlaceEnemy aid (toPlacement placement)

instance Placeable EnemyAttrs where
  place attrs placement = push $ PlaceEnemy attrs.id (toPlacement placement)

instance Placeable TreacheryId where
  place tid placement = push $ PlaceTreachery tid (toPlacement placement)

instance Placeable SkillAttrs where
  place attrs placement = push $ PlaceSkill attrs.id (toPlacement placement)

instance Placeable InvestigatorId where
  place iid placement = push $ PlaceInvestigator iid (toPlacement placement)
