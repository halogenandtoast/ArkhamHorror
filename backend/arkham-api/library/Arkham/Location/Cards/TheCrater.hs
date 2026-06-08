module Arkham.Location.Cards.TheCrater (theCrater) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheBlobThatAteEverything.Helpers
import Arkham.Token qualified as Token
import Arkham.Trait (Trait (Manifold))

newtype TheCrater = TheCrater LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCrater :: LocationCard TheCrater
theCrater = locationWith TheCrater Cards.theCrater 1 (PerPlayer 2) connectsToAdjacent

instance HasAbilities TheCrater where
  getAbilities (TheCrater a) =
    extendRevealed
      a
      [ scenarioI18n
          $ withI18nTooltip "theCrater.deal"
          $ restricted a 1 (exists targetEnemy)
          $ actionAbilityWithCost (SpendTokenCost Token.Resource (TargetIs ScenarioTarget))
      , mkAbility a 2 $ forced $ RoundEnds #when
      ]
   where
    targetEnemy = oneOf [enemyIs Enemies.subject8L08, EnemyWithTrait Manifold]

instance RunMessage TheCrater where
  runMessage msg l@(TheCrater attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ oneOf [enemyIs Enemies.subject8L08, EnemyWithTrait Manifold]
      chooseTargetM iid enemies \enemy ->
        nonAttackEnemyDamage (Just iid) (attrs.ability 1) 3 enemy
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      iids <- select $ investigatorAt attrs
      for_ iids \iid -> assignDamage iid (attrs.ability 2) 1
      pure l
    _ -> TheCrater <$> liftRunMessage msg attrs
