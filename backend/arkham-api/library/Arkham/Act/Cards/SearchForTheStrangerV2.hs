module Arkham.Act.Cards.SearchForTheStrangerV2 (searchForTheStrangerV2) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Placement

newtype SearchForTheStrangerV2 = SearchForTheStrangerV2 ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

searchForTheStrangerV2 :: ActCard SearchForTheStrangerV2
searchForTheStrangerV2 = act (2, A) SearchForTheStrangerV2 Cards.searchForTheStrangerV2 Nothing

instance HasModifiersFor SearchForTheStrangerV2 where
  getModifiersFor (SearchForTheStrangerV2 a) = do
    modifySelectMap a (enemyIs Enemies.theManInThePallidMask) \eid ->
      [CanOnlyBeDefeatedBy (SourceIs $ EnemySource eid)]
    modifySelect a Anyone [CannotDiscoverClues]

instance HasAbilities SearchForTheStrangerV2 where
  getAbilities (SearchForTheStrangerV2 x) =
    [mkAbility x 1 $ forced $ EnemyWouldBeDefeated #when $ enemyIs Enemies.theManInThePallidMask]

instance RunMessage SearchForTheStrangerV2 where
  runMessage msg a@(SearchForTheStrangerV2 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      hastur <- getSetAsideCard Enemies.hasturTheKingInYellow
      createEnemy_ hastur Global
      theManInThePallidMask <- selectJust (enemyIs Enemies.theManInThePallidMask)
      removeFromGame theManInThePallidMask
      advanceActDeck attrs
      pure a
    _ -> SearchForTheStrangerV2 <$> liftRunMessage msg attrs
