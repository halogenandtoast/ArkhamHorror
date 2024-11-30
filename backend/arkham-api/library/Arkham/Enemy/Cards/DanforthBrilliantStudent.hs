module Arkham.Enemy.Cards.DanforthBrilliantStudent (
  danforthBrilliantStudent,
  DanforthBrilliantStudent (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Scenario (getScenarioDeck)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Deck

newtype DanforthBrilliantStudent = DanforthBrilliantStudent EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

danforthBrilliantStudent :: EnemyCard DanforthBrilliantStudent
danforthBrilliantStudent = enemy DanforthBrilliantStudent Cards.danforthBrilliantStudent (0, Static 1, 0) (0, 0)

instance HasAbilities DanforthBrilliantStudent where
  getAbilities (DanforthBrilliantStudent a) =
    extend
      a
      [ restricted a 1 OnSameLocation parleyAction_
      , mkAbility a 2 $ forced $ EnemyDefeated #if Anyone ByAny (be a)
      ]

instance RunMessage DanforthBrilliantStudent where
  runMessage msg e@(DanforthBrilliantStudent attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      maxN <- min 2 . length <$> getScenarioDeck TekeliliDeck
      chooseOneM iid do
        for_ [0 .. maxN] \n -> do
          labeled ("Draw " <> tshow n <> " cards from the Tekelili Deck") $ do
            placeTokens (attrs.ability 1) attrs #resource n
            doStep 2 msg
      pure e
    DoStep 2 (UseThisAbility _iid (isSource attrs -> True) 1) -> do
      n <- perPlayer 3
      when (attrs.token #resource >= n) $ addToVictory attrs
      pure e
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      partnerEliminated Assets.danforthBrilliantStudent
      pure e
    _ -> DanforthBrilliantStudent <$> liftRunMessage msg attrs
