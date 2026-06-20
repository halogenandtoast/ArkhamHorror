module Arkham.Enemy.Cards.BroodQueenDyingMother (broodQueenDyingMother) where

import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers (Day (..), getCampaignDay)
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Trait (Trait (Insect))

newtype BroodQueenDyingMother = BroodQueenDyingMother EnemyAttrs
  deriving anyclass (IsEnemy, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

broodQueenDyingMother :: EnemyCard BroodQueenDyingMother
broodQueenDyingMother = enemy BroodQueenDyingMother Cards.broodQueenDyingMother

instance HasModifiersFor BroodQueenDyingMother where
  getModifiersFor (BroodQueenDyingMother a) = do
    insectCount <- selectCount $ EnemyWithTrait Insect <> not_ (be a)
    playerCount <- getPlayerCount
    day <- getCampaignDay
    let dayNum = case day of
          Day1 -> 1
          Day2 -> 2
          Day3 -> 3
    modifySelect
      a
      (EnemyAt (locationWithEnemy a) <> EnemyWithTrait Insect)
      [RemoveKeyword Keyword.Aloof]
    modifySelf a [EnemyFight insectCount, HealthModifier (dayNum * playerCount)]
