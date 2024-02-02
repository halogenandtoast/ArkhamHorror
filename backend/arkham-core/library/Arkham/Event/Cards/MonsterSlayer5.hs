module Arkham.Event.Cards.MonsterSlayer5 (
  monsterSlayer5,
  MonsterSlayer5 (..),
) where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Enemy
import Arkham.Projection
import Arkham.SkillType
import Arkham.Trait

newtype MonsterSlayer5 = MonsterSlayer5 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

monsterSlayer5 :: EventCard MonsterSlayer5
monsterSlayer5 = event MonsterSlayer5 Cards.monsterSlayer5

instance RunMessage MonsterSlayer5 where
  runMessage msg e@(MonsterSlayer5 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      push $ ChooseFightEnemy iid (toSource attrs) Nothing SkillCombat mempty False
      pure e
    PassedSkillTest iid (Just Action.Fight) (isSource attrs -> True) SkillTestInitiatorTarget {} _ _ ->
      do
        mTarget <- getSkillTestTarget
        case mTarget of
          Just (EnemyTarget eid) -> do
            traits <- field EnemyTraits eid
            when (Elite `notMember` traits) (pushAllM $ defeatEnemy eid iid attrs)
          _ -> error "impossible"
        pure e
    _ -> MonsterSlayer5 <$> runMessage msg attrs
