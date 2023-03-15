module Arkham.Event.Cards.Counterpunch
  ( counterpunch
  , Counterpunch(..)
  ) where

import Arkham.Prelude

import Arkham.Attack
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window

newtype Counterpunch = Counterpunch EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

counterpunch :: EventCard Counterpunch
counterpunch = event Counterpunch Cards.counterpunch

toEnemy :: [Window] -> EnemyMatcher
toEnemy [] = error "invalid call"
toEnemy (Window _ (Window.EnemyAttacksEvenIfCancelled details) : _) =
  attackEnemy details
toEnemy (_ : xs) = toEnemy xs

instance RunMessage Counterpunch where
  runMessage msg e@(Counterpunch attrs) = case msg of
    InvestigatorPlayEvent iid eid _ windows' _ | eid == toId attrs -> do
      mEnemy <- selectOne $ toEnemy windows'
      for_ mEnemy $ \enemy ->
        push $ FightEnemy iid enemy (toSource attrs) Nothing SkillCombat False
      pure e
    _ -> Counterpunch <$> runMessage msg attrs
