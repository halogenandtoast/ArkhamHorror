module Arkham.Enemy.Cards.CatsOfUlthar (catsOfUlthar) where

import Arkham.Ability
import Arkham.Campaigns.TheDreamEaters.Helpers
import Arkham.Campaigns.TheDreamEaters.Key
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated)
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified_)
import Arkham.Helpers.SkillTest (getSkillTest)
import Arkham.Matcher
import Arkham.Message.Lifted.Log

newtype CatsOfUlthar = CatsOfUlthar EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

catsOfUlthar :: EnemyCard CatsOfUlthar
catsOfUlthar = enemy CatsOfUlthar Cards.catsOfUlthar (1, Static 1, 1) (1, 0)

-- This is a bit weird to handle, but this is the only card that does that so
-- we affect the first token drawn and treat it like you need to draw an
-- additional. It might be better in the future to move this as a modifier
-- directly on the skill test
instance HasModifiersFor CatsOfUlthar where
  getModifiersFor (CatsOfUlthar a) = do
    getSkillTest >>= traverse_ \st -> do
      maybeModified_ a (SkillTestTarget st.id) do
        guard $ a `is` st.target && st.action == Just #evade
        pure [RevealAnotherChaosToken]

instance HasAbilities CatsOfUlthar where
  getAbilities (CatsOfUlthar x) =
    extend1 x
      $ restrict (notExists $ DefeatedEnemy $ enemyIs Cards.catsOfUlthar)
      $ forcedAbility x 1 (EnemyDefeated #when Anyone ByAny $ be x)

instance RunMessage CatsOfUlthar where
  runMessage msg e@(CatsOfUlthar attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      host <- selectJust $ enemyIs Cards.catsOfUlthar <> not_ IsSwarm
      when (host == attrs.id) $ cancelEnemyDefeat attrs.id
      placeSwarmCards host 3
      recordForInvestigator iid HasBrokenTheLawOfUlthar
      pure e
    _ -> CatsOfUlthar <$> liftRunMessage msg attrs
