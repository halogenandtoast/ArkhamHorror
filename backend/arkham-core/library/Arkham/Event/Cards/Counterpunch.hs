module Arkham.Event.Cards.Counterpunch (counterpunch, Counterpunch (..)) where

import Arkham.Attack
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Fight
import Arkham.Helpers.Window (getAttackDetails)

newtype Counterpunch = Counterpunch EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

counterpunch :: EventCard Counterpunch
counterpunch = event Counterpunch Cards.counterpunch

instance RunMessage Counterpunch where
  runMessage msg e@(Counterpunch attrs) = runQueueT $ case msg of
    InvestigatorPlayEvent iid (is attrs -> True) _ (attackEnemy . getAttackDetails -> enemy) _ -> do
      pushM $ mkFightEnemy iid attrs enemy
      pure e
    _ -> Counterpunch <$> liftRunMessage msg attrs
