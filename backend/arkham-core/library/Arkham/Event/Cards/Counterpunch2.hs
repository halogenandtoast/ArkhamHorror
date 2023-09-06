module Arkham.Event.Cards.Counterpunch2 (
  counterpunch2,
  Counterpunch2 (..),
) where

import Arkham.Prelude

import Arkham.Attack
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Id
import Arkham.Message
import Arkham.SkillType
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype Counterpunch2 = Counterpunch2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

counterpunch2 :: EventCard Counterpunch2
counterpunch2 = event Counterpunch2 Cards.counterpunch2

toEnemy :: [Window] -> EnemyId
toEnemy [] = error "invalid call"
toEnemy ((windowType -> Window.EnemyAttacks details) : _) = attackEnemy details
toEnemy (_ : xs) = toEnemy xs

instance RunMessage Counterpunch2 where
  runMessage msg e@(Counterpunch2 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ (toEnemy -> enemy) _ | eid == toId attrs -> do
      pushAll
        [ skillTestModifiers
            attrs
            (InvestigatorTarget iid)
            [SkillModifier SkillCombat 2, DamageDealt 1]
        , FightEnemy iid enemy (toSource attrs) Nothing SkillCombat False
        ]
      pure e
    _ -> Counterpunch2 <$> runMessage msg attrs
