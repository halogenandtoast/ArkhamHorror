module Arkham.Enemy.Cards.Subject8L08 (subject8L08) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Modifier (UIModifier (Oversized))

newtype Subject8L08 = Subject8L08 EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

instance HasModifiersFor Subject8L08 where
  getModifiersFor (Subject8L08 a) = modifySelf a [UIModifier Oversized, HealthModifier 15]

-- Subject 8L-08 has no fight, evade, damage, or horror values. Its health is
-- 15 per investigator (Single Group mode).
subject8L08 :: EnemyCard Subject8L08
subject8L08 =
  enemyWith Subject8L08 Cards.subject8L08 (0, PerPlayer 15, 0) (0, 0)
    $ \a -> a {enemyFight = Nothing, enemyEvade = Nothing}

instance RunMessage Subject8L08 where
  runMessage msg e@(Subject8L08 attrs) = runQueueT $ case msg of
    ScenarioSpecific "devour" (maybeResult -> Just target) -> do
      case target of
        LocationTarget lid -> do
          card <- fetchCard lid
          removeLocation lid
          obtainCard card
          placeUnderneath attrs [card]
        _ -> pure ()
      pure e
    _ -> Subject8L08 <$> liftRunMessage msg attrs
