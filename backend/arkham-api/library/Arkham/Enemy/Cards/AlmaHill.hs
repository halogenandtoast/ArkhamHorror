module Arkham.Enemy.Cards.AlmaHill (AlmaHill (..), almaHill) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Deck
import Arkham.Draw.Types
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Prelude

newtype AlmaHill = AlmaHill EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

almaHill :: EnemyCard AlmaHill
almaHill = enemyWith AlmaHill Cards.almaHill (3, Static 3, 3) (0, 2) $ spawnAtL ?~ "Southside"

instance HasAbilities AlmaHill where
  getAbilities (AlmaHill attrs) = extend attrs [restrictedAbility attrs 1 OnSameLocation parleyAction_]

instance RunMessage AlmaHill where
  runMessage msg e@(AlmaHill attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ DrawCards iid $ newCardDraw attrs EncounterDeck 3 `andThen` AddToVictory (toTarget attrs)
      pure e
    _ -> AlmaHill <$> runMessage msg attrs
