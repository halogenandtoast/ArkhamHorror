module Arkham.Types.Enemy.Cards.YithianStarseeker
  ( yithianStarseeker
  , YithianStarseeker(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationMatcher
import Arkham.Types.Message
import Arkham.Types.Query

newtype YithianStarseeker = YithianStarseeker EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yithianStarseeker :: EnemyCard YithianStarseeker
yithianStarseeker = enemyWith
  YithianStarseeker
  Cards.yithianStarseeker
  (3, Static 4, 5)
  (2, 1)
  (spawnAtL ?~ LocationWithTitle "Another Dimension")

instance HasModifiersFor env YithianStarseeker

instance EnemyAttrsHasActions env => HasActions env YithianStarseeker where
  getActions i window (YithianStarseeker attrs) = getActions i window attrs

instance (HasCount DiscardCount env InvestigatorId, EnemyAttrsRunMessage env) => RunMessage env YithianStarseeker where
  runMessage msg (YithianStarseeker attrs) = case msg of
    PerformEnemyAttack iid eid | eid == enemyId attrs -> do
      discardCount <- unDiscardCount <$> getCount iid
      YithianStarseeker <$> runMessage
        msg
        (attrs & doomL +~ if discardCount >= 10 then 1 else 0)
    _ -> YithianStarseeker <$> runMessage msg attrs
