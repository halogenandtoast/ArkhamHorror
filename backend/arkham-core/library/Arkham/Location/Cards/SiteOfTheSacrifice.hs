module Arkham.Location.Cards.SiteOfTheSacrifice
  ( siteOfTheSacrifice
  , SiteOfTheSacrifice(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Attack
import Arkham.Cost
import Arkham.Criteria
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Helpers.Enemy
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing

newtype SiteOfTheSacrifice = SiteOfTheSacrifice LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

siteOfTheSacrifice :: LocationCard SiteOfTheSacrifice
siteOfTheSacrifice =
  location SiteOfTheSacrifice Cards.siteOfTheSacrifice 4 (PerPlayer 3)

instance HasAbilities SiteOfTheSacrifice where
  getAbilities (SiteOfTheSacrifice a) = withBaseAbilities a
    [ doesNotProvokeAttacksOfOpportunity
      $ restrictedAbility a 1
        (Here <> enemyExists (enemyIs Enemies.nahab <> EnemyWithAnyDoom)
        )
      $ ActionAbility Nothing
      $ ActionCost 1 <> GroupClueCost (PerPlayer 1) (LocationWithId $ toId a)

    , restrictedAbility a 2 (CluesOnThis $ LessThan $ PerPlayer 3)
      $ ForcedAbility
      $ RoundEnds Timing.When
    , haunted "You must either place 1 doom on Nahab, or Nahab attacks you." a 3
    ]

instance RunMessage SiteOfTheSacrifice where
  runMessage msg l@(SiteOfTheSacrifice attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      nahab <- getUniqueEnemy Enemies.nahab
      push $ RemoveDoom (toTarget nahab) 1
      pure l
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      cluesToAdd <-
        max 0 . subtract (locationClues attrs) <$> perPlayer 3
      push $ PlaceClues (toTarget attrs) cluesToAdd
      pure l
    UseCardAbility iid (isSource attrs -> True) 3 _ _ -> do
      nahab <- getUniqueEnemy Enemies.nahab
      push $ chooseOne iid
        [ Label "Place 1 doom on Nahab" [PlaceDoom (toTarget nahab) 1]
        , Label "Nahab attacks you" [InitiateEnemyAttack $ enemyAttack nahab iid]
        ]
      pure l
    _ -> SiteOfTheSacrifice <$> runMessage msg attrs
