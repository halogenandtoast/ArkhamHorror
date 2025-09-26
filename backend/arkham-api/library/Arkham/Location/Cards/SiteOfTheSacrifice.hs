module Arkham.Location.Cards.SiteOfTheSacrifice (siteOfTheSacrifice) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Helpers.Enemy
import Arkham.Helpers.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheSecretName.Helpers

newtype SiteOfTheSacrifice = SiteOfTheSacrifice LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

siteOfTheSacrifice :: LocationCard SiteOfTheSacrifice
siteOfTheSacrifice = location SiteOfTheSacrifice Cards.siteOfTheSacrifice 4 (PerPlayer 3)

instance HasAbilities SiteOfTheSacrifice where
  getAbilities (SiteOfTheSacrifice a) =
    extendRevealed
      a
      [ doesNotProvokeAttacksOfOpportunity
          $ restricted a 1 (Here <> exists (enemyIs Enemies.nahab <> EnemyWithAnyDoom))
          $ actionAbilityWithCost (GroupClueCost (PerPlayer 1) (be a))
      , restricted a 2 (CluesOnThis $ LessThan $ PerPlayer 3) $ forced $ RoundEnds #when
      , scenarioI18n $ hauntedI "siteOfTheSacrifice.haunted" a 3
      ]

instance RunMessage SiteOfTheSacrifice where
  runMessage msg l@(SiteOfTheSacrifice attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      nahab <- getUniqueEnemy Enemies.nahab
      removeDoom (attrs.ability 1) nahab 1
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      cluesToAdd <-
        max 0 . subtract attrs.clues <$> perPlayer 3
      placeClues (attrs.ability 2) attrs cluesToAdd
      pure l
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      nahab <- getUniqueEnemy Enemies.nahab
      chooseOneM iid $ scenarioI18n do
        labeled' "placeDoom" $ placeDoom (attrs.ability 3) nahab 1
        labeled' "nahabAttacks" $ initiateEnemyAttack nahab attrs iid
      pure l
    _ -> SiteOfTheSacrifice <$> liftRunMessage msg attrs
