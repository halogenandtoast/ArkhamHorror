module Arkham.Types.Enemy.Cards.YithianStarseeker
  ( yithianStarseeker
  , YithianStarseeker(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.InvestigatorId
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Query

newtype YithianStarseeker = YithianStarseeker EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yithianStarseeker :: EnemyCard YithianStarseeker
yithianStarseeker = enemyWith
  YithianStarseeker
  Cards.yithianStarseeker
  (3, Static 4, 5)
  (2, 1)
  (spawnAtL ?~ LocationWithTitle "Another Dimension")

instance HasModifiersFor env YithianStarseeker

instance EnemyAttrsHasAbilities env => HasAbilities env YithianStarseeker where
  getAbilities i window (YithianStarseeker attrs) = getAbilities i window attrs

instance (HasCount DiscardCount env InvestigatorId, EnemyAttrsRunMessage env) => RunMessage env YithianStarseeker where
  runMessage msg (YithianStarseeker attrs) = case msg of
    PerformEnemyAttack iid eid _ | eid == enemyId attrs -> do
      discardCount <- unDiscardCount <$> getCount iid
      YithianStarseeker <$> runMessage
        msg
        (attrs & doomL +~ if discardCount >= 10 then 1 else 0)
    _ -> YithianStarseeker <$> runMessage msg attrs
