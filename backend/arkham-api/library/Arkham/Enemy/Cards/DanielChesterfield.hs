module Arkham.Enemy.Cards.DanielChesterfield (danielChesterfield) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype DanielChesterfield = DanielChesterfield EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

danielChesterfield :: EnemyCard DanielChesterfield
danielChesterfield =
  enemy DanielChesterfield Cards.danielChesterfield (3, Static 4, 3) (1, 1)
    & setPrey (InvestigatorWithHighestSkill #combat UneliminatedInvestigator)

instance HasAbilities DanielChesterfield where
  getAbilities (DanielChesterfield x) =
    extend1 x
      $ restricted
        x
        1
        (OnSameLocation <> exists (AssetControlledBy You <> assetIs Assets.claspOfBlackOnyx))
        parleyAction_

instance RunMessage DanielChesterfield where
  runMessage msg a@(DanielChesterfield attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      addToVictory attrs
      pure a
    _ -> DanielChesterfield <$> liftRunMessage msg attrs
