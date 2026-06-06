module Arkham.Enemy.Cards.Subject8L08 (subject8L08) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Modifier (UIModifier (Oversized))

newtype Subject8L08 = Subject8L08 EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor Subject8L08 where
  getModifiersFor (Subject8L08 a) = modifySelf a [UIModifier Oversized, HealthModifier 15]

instance HasAbilities Subject8L08 where
  getAbilities (Subject8L08 a) = [mkAbility a 1 $ Objective $ forced $ EnemyDefeated #when Anyone ByAny (be a)]

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
        AssetTarget aid -> do
          card <- fetchCard aid
          removeAsset aid
          obtainCard card
          placeUnderneath attrs [card]
        _ -> pure ()
      pure e
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      push R2
      pure e
    _ -> Subject8L08 <$> liftRunMessage msg attrs
