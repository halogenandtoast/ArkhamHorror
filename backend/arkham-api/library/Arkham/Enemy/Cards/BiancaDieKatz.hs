module Arkham.Enemy.Cards.BiancaDieKatz (biancaDieKatz, BiancaDieKatz (..)) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype Meta = Meta {forcedDisabled :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype BiancaDieKatz = BiancaDieKatz (EnemyAttrs `With` Meta)
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

biancaDieKatz :: EnemyCard BiancaDieKatz
biancaDieKatz =
  enemyWith
    (BiancaDieKatz . (`with` Meta False))
    Cards.biancaDieKatz
    (3, Static 3, 3)
    (2, 0)
    (\a -> a & preyL .~ BearerOf (toId a))

instance HasAbilities BiancaDieKatz where
  getAbilities (BiancaDieKatz (With attrs meta)) =
    extend
      attrs
      [ restrictedAbility attrs 1 IfYouOweBiancaDieKatz
          $ parleyAction (CalculatedResourceCost (AmountYouOweToBiancaDieKatz You))
      , restrictedAbility attrs 2 (OwnsThis <> criteria) $ forced $ EnemyLeavesPlay #when (be attrs)
      ]
   where
    criteria = if forcedDisabled meta then Never else NoRestriction

instance RunMessage BiancaDieKatz where
  runMessage msg e@(BiancaDieKatz (With attrs meta)) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      addToVictory attrs
      pure . BiancaDieKatz $ attrs `with` Meta True
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      obtainCard attrs
      shuffleIntoDeck iid attrs
      pure e
    _ -> BiancaDieKatz . (`with` meta) <$> liftRunMessage msg attrs
