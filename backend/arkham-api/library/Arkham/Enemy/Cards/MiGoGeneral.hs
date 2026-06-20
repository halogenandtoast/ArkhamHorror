module Arkham.Enemy.Cards.MiGoGeneral (miGoGeneral) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Matcher

newtype MiGoGeneral = MiGoGeneral EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miGoGeneral :: EnemyCard MiGoGeneral
miGoGeneral = enemy MiGoGeneral Cards.miGoGeneral

instance HasAbilities MiGoGeneral where
  getAbilities (MiGoGeneral a) = [mkAbility a 1 $ forced $ EnemyAttacks #after You AnyEnemyAttack (be a)]

instance RunMessage MiGoGeneral where
  runMessage msg e@(MiGoGeneral attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      fungusMound <- selectJust $ LocationWithTitle "Fungus Mound"
      whenMatch iid InvestigatorWithAnyClues $ moveTokens (attrs.ability 1) iid fungusMound #clue 1
      pure e
    _ -> MiGoGeneral <$> liftRunMessage msg attrs
