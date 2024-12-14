module Arkham.Enemy.Cards.SeepingNightmare (seepingNightmare) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated)
import Arkham.Helpers.Modifiers
import Arkham.Keyword
import Arkham.Matcher
import Arkham.Trait (Trait (Eidolon))

newtype SeepingNightmare = SeepingNightmare EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

seepingNightmare :: EnemyCard SeepingNightmare
seepingNightmare = enemy SeepingNightmare Cards.seepingNightmare (3, Static 6, 3) (2, 2)

instance HasModifiersFor SeepingNightmare where
  getModifiersFor (SeepingNightmare a) = do
    when (null $ enemyCardsUnderneath a) $ modifySelf a [AddKeyword Hunter]
    modifySelectMap
      a
      (LocationWithEnemy $ MovingEnemy <> EnemyAt (locationWithEnemy a) <> EnemyWithTrait Eidolon)
      \lid -> [ConnectedToWhen (LocationWithModifier $ ScenarioModifier "camp") (LocationWithId lid)]

instance HasAbilities SeepingNightmare where
  getAbilities (SeepingNightmare a) =
    extend1 a $ restricted a 1 criteria $ forced $ EnemyDefeated #when Anyone ByAny (be a)
   where
    criteria = if null (enemyCardsUnderneath a) then Never else NoRestriction

instance RunMessage SeepingNightmare where
  runMessage msg a@(SeepingNightmare attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      for_ (enemyCardsUnderneath attrs) obtainCard
      pure a
    _ -> SeepingNightmare <$> liftRunMessage msg attrs
