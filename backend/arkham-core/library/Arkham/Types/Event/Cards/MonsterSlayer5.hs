module Arkham.Types.Event.Cards.MonsterSlayer5
  ( monsterSlayer5
  , MonsterSlayer5(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import qualified Arkham.Types.Action as Action
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.SkillTest
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Trait

newtype MonsterSlayer5 = MonsterSlayer5 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

monsterSlayer5 :: EventCard MonsterSlayer5
monsterSlayer5 = event MonsterSlayer5 Cards.monsterSlayer5

instance (HasSet Trait env EnemyId, HasSkillTest env) => RunMessage env MonsterSlayer5 where
  runMessage msg e@(MonsterSlayer5 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == toId attrs -> do
      e <$ pushAll
        [ ChooseFightEnemy iid (toSource attrs) Nothing SkillCombat mempty False
        , Discard (toTarget attrs)
        ]
    PassedSkillTest iid (Just Action.Fight) source SkillTestInitiatorTarget{} _ _
      | isSource attrs source
      -> do
        mTarget <- getSkillTestTarget
        e <$ case mTarget of
          Just (EnemyTarget eid) -> do
            traits <- getSet eid
            when (Elite `notMember` traits) (push $ DefeatEnemy eid iid source)
          _ -> error "impossible"
    _ -> MonsterSlayer5 <$> runMessage msg attrs
