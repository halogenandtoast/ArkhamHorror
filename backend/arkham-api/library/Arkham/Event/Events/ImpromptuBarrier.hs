module Arkham.Event.Events.ImpromptuBarrier (impromptuBarrier) where

import Arkham.Action qualified as Action
import Arkham.Enemy.Types qualified as Enemy (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Modifier (ModifierType (..))

newtype ImpromptuBarrier = ImpromptuBarrier EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

impromptuBarrier :: EventCard ImpromptuBarrier
impromptuBarrier = event ImpromptuBarrier Cards.impromptuBarrier

instance RunMessage ImpromptuBarrier where
  runMessage msg e@(ImpromptuBarrier attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      chooseEvadeEnemyEdit sid iid attrs (setTarget attrs)
      when attrs.playedFromDiscard $ shuffleIntoDeck iid attrs
      pure e
    ChosenEvadeEnemy sid (isSource attrs -> True) eid -> do
      skillTestModifier sid attrs eid (EnemyEvade (-1))
      pure e
    Successful (Action.Evade, EnemyTarget enemyId) iid source (isTarget attrs -> True) n -> do
      push $ Successful (#evade, EnemyTarget enemyId) iid source (toTarget enemyId) n
      enemies <- select $ EnemyWithMaybeFieldLessThanOrEqualTo n Enemy.EnemyEvade <> not_ (be enemyId)
      chooseOrRunOneM iid do
        questionLabeled "Evade another enemy"
        questionLabeledCard attrs
        labeled "Do not evade another enemy" nothing
        targets enemies (automaticallyEvadeEnemy iid)

      pure e
    _ -> ImpromptuBarrier <$> liftRunMessage msg attrs
