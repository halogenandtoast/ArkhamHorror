module Arkham.Event.Cards.Counterpunch (
  counterpunch,
  Counterpunch (..),
) where

import Arkham.Prelude

import Arkham.Attack
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Id
import Arkham.SkillType
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype Counterpunch = Counterpunch EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

counterpunch :: EventCard Counterpunch
counterpunch = event Counterpunch Cards.counterpunch

toEnemy :: [Window] -> EnemyId
toEnemy [] = error "invalid call"
toEnemy ((windowType -> Window.EnemyAttacksEvenIfCancelled details) : _) =
  attackEnemy details
toEnemy (_ : xs) = toEnemy xs

instance RunMessage Counterpunch where
  runMessage msg e@(Counterpunch attrs) = case msg of
    InvestigatorPlayEvent iid eid _ (toEnemy -> enemy) _ | eid == toId attrs -> do
      push $ FightEnemy iid enemy (toSource attrs) Nothing SkillCombat False
      pure e
    _ -> Counterpunch <$> runMessage msg attrs
