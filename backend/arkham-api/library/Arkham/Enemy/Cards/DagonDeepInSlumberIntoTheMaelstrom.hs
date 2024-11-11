module Arkham.Enemy.Cards.DagonDeepInSlumberIntoTheMaelstrom (
  dagonDeepInSlumberIntoTheMaelstrom,
  DagonDeepInSlumberIntoTheMaelstrom (..),
)
where

import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Message (ReplaceStrategy (..))

newtype DagonDeepInSlumberIntoTheMaelstrom = DagonDeepInSlumberIntoTheMaelstrom EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

dagonDeepInSlumberIntoTheMaelstrom :: EnemyCard DagonDeepInSlumberIntoTheMaelstrom
dagonDeepInSlumberIntoTheMaelstrom = enemyWith
  DagonDeepInSlumberIntoTheMaelstrom
  Cards.dagonDeepInSlumberIntoTheMaelstrom
  (0, Static 1, 0)
  (0, 0)
  $ \a -> a {enemyFight = Nothing, enemyHealth = Nothing, enemyEvade = Nothing}

instance HasModifiersFor DagonDeepInSlumberIntoTheMaelstrom where
  getModifiersFor target (DagonDeepInSlumberIntoTheMaelstrom a) = maybeModified a do
    guard $ isTarget a target
    pure [Omnipotent]

instance RunMessage DagonDeepInSlumberIntoTheMaelstrom where
  runMessage msg e@(DagonDeepInSlumberIntoTheMaelstrom attrs) = runQueueT $ case msg of
    EnemyCheckEngagement eid | eid == attrs.id -> pure e
    Flip _ _ (isTarget attrs -> True) -> do
      awakened <- genCard Cards.dagonAwakenedAndEnragedIntoTheMaelstrom
      push $ ReplaceEnemy attrs.id awakened Swap
      pure e
    _ -> DagonDeepInSlumberIntoTheMaelstrom <$> liftRunMessage msg attrs
