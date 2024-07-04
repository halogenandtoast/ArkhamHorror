module Arkham.Event.Cards.Counterpunch2 (counterpunch2, Counterpunch2 (..)) where

import Arkham.Attack
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Fight
import Arkham.Helpers.Window (getAttackDetails)
import Arkham.Modifier

newtype Counterpunch2 = Counterpunch2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

counterpunch2 :: EventCard Counterpunch2
counterpunch2 = event Counterpunch2 Cards.counterpunch2

instance RunMessage Counterpunch2 where
  runMessage msg e@(Counterpunch2 attrs) = runQueueT $ case msg of
    InvestigatorPlayEvent iid (is attrs -> True) _ (attackEnemy . getAttackDetails -> enemy) _ -> do
      skillTestModifiers attrs iid [SkillModifier #combat 2, DamageDealt 1]
      pushM $ mkFightEnemy iid attrs enemy
      pure e
    _ -> Counterpunch2 <$> liftRunMessage msg attrs
