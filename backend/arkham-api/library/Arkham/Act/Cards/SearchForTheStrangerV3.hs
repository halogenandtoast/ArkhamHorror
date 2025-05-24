module Arkham.Act.Cards.SearchForTheStrangerV3 (searchForTheStrangerV3) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype SearchForTheStrangerV3 = SearchForTheStrangerV3 ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

searchForTheStrangerV3 :: ActCard SearchForTheStrangerV3
searchForTheStrangerV3 = act (2, A) SearchForTheStrangerV3 Cards.searchForTheStrangerV3 Nothing

instance HasModifiersFor SearchForTheStrangerV3 where
  getModifiersFor (SearchForTheStrangerV3 a) = do
    modifySelect a (enemyIs Enemies.theManInThePallidMask) [CanOnlyBeDefeatedByDamage]
    modifySelect a Anyone [CannotDiscoverClues]

instance HasAbilities SearchForTheStrangerV3 where
  getAbilities (SearchForTheStrangerV3 x) =
    [mkAbility x 1 $ forced $ EnemyWouldBeDefeated #when $ enemyIs Enemies.theManInThePallidMask]

instance RunMessage SearchForTheStrangerV3 where
  runMessage msg a@(SearchForTheStrangerV3 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      hastur <- getSetAsideCard Enemies.hasturLordOfCarcosa
      theManInThePallidMask <- selectJust (enemyIs Enemies.theManInThePallidMask)
      location <- selectJust $ locationWithEnemy theManInThePallidMask
      createEnemyAt_ hastur location
      removeFromGame theManInThePallidMask
      advanceActDeck attrs
      pure a
    _ -> SearchForTheStrangerV3 <$> liftRunMessage msg attrs
