module Arkham.Enemy.Cards.DanforthBrilliantStudent (danforthBrilliantStudent) where

import Arkham.Ability
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Matcher

newtype DanforthBrilliantStudent = DanforthBrilliantStudent EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

danforthBrilliantStudent :: EnemyCard DanforthBrilliantStudent
danforthBrilliantStudent = enemy DanforthBrilliantStudent Cards.danforthBrilliantStudent (2, Static 2, 4) (0, 2)

instance HasAbilities DanforthBrilliantStudent where
  getAbilities (DanforthBrilliantStudent a) =
    extend
      a
      [ restricted a 1 OnSameLocation parleyAction_
      , mkAbility a 2 $ forced $ EnemyDefeated #when Anyone ByAny (be a)
      ]

instance RunMessage DanforthBrilliantStudent where
  runMessage msg e@(DanforthBrilliantStudent attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseAmounts
        iid
        "Number of cards to draw from the top of the Tekeli-li deck"
        (MaxAmountTarget 2)
        [("Cards", (0, 2))]
        attrs
      pure e
    ResolveAmounts iid (getChoiceAmount "Cards" -> n) (isTarget attrs -> True) -> do
      drawTekelili iid (attrs.ability 1) n
      placeTokens (attrs.ability 1) attrs #resource n
      doStep 2 msg
      pure e
    DoStep 2 (ResolveAmounts _ _ (isTarget attrs -> True)) -> do
      n <- perPlayer 3
      when (attrs.token #resource >= n) $ addToVictory attrs
      pure e
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      eliminatePartner attrs
      pure e
    _ -> DanforthBrilliantStudent <$> liftRunMessage msg attrs
