module Arkham.Enemy.Cards.DanielChesterfield (danielChesterfield, DanielChesterfield (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype DanielChesterfield = DanielChesterfield EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

danielChesterfield :: EnemyCard DanielChesterfield
danielChesterfield =
  enemyWith DanielChesterfield Cards.danielChesterfield (3, Static 4, 3) (1, 1)
    $ preyL
    .~ Prey (InvestigatorWithHighestSkill #combat UneliminatedInvestigator)

instance HasAbilities DanielChesterfield where
  getAbilities (DanielChesterfield x) =
    extend
      x
      [ restrictedAbility
          x
          1
          (OnSameLocation <> exists (AssetControlledBy You <> assetIs Assets.claspOfBlackOnyx))
          parleyAction_
      ]

instance RunMessage DanielChesterfield where
  runMessage msg a@(DanielChesterfield attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ AddToVictory $ toTarget attrs
      pure a
    _ -> DanielChesterfield <$> runMessage msg attrs
