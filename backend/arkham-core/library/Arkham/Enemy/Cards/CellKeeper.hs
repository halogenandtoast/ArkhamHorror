module Arkham.Enemy.Cards.CellKeeper (
  cellKeeper,
  CellKeeper (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Key
import Arkham.Matcher
import Arkham.Message
import Arkham.Scenarios.ForTheGreaterGood.Helpers
import Arkham.Timing qualified as Timing
import Arkham.Trait (Trait (Sanctum))

newtype CellKeeper = CellKeeper EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cellKeeper :: EnemyCard CellKeeper
cellKeeper =
  enemyWith
    CellKeeper
    Cards.cellKeeper
    (3, Static 3, 2)
    (0, 2)
    (spawnAtL ?~ SpawnLocation (LocationWithTrait Sanctum))

instance HasAbilities CellKeeper where
  getAbilities (CellKeeper attrs) =
    withBaseAbilities
      attrs
      [ mkAbility attrs 1 $ ForcedAbility $ EnemySpawns Timing.After Anywhere $ EnemyWithId $ toId attrs
      , restrictedAbility attrs 2 keyCriteria $
          ForcedAbility $
            SkillTestResult
              Timing.After
              You
              (WhileEvadingAnEnemy $ EnemyWithId $ toId attrs)
              (SuccessResult $ AtLeast $ Static 2)
      ]
   where
    keyCriteria = if null (enemyKeys attrs) then Never else NoRestriction

instance RunMessage CellKeeper where
  runMessage msg e@(CellKeeper attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      mKey <- getRandomKey
      pushAll $
        PlaceDoom (toAbilitySource attrs 1) (toTarget attrs) 2
          : [PlaceKey (toTarget attrs) k | k <- maybeToList mKey]
      pure e
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      push $
        chooseOrRunOne
          iid
          [ Label ("Take control of " <> keyName k <> " key") [PlaceKey (toTarget iid) k]
          | k <- setToList (enemyKeys attrs)
          ]
      pure e
    _ -> CellKeeper <$> runMessage msg attrs
