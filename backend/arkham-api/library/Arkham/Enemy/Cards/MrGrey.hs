module Arkham.Enemy.Cards.MrGrey (mrGrey) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher
import Arkham.Scenarios.ByTheBook.Helpers

newtype MrGrey = MrGrey EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mrGrey :: EnemyCard MrGrey
mrGrey =
  enemyWith MrGrey Cards.mrGrey
    $ spawnAtL
    ?~ SpawnEngagedWith rolandBanks

instance HasModifiersFor MrGrey where
  getModifiersFor (MrGrey a) = do
    n <- perPlayer 2
    modifySelf a [HealthModifier n]

instance HasAbilities MrGrey where
  getAbilities (MrGrey a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyAttacks #after You AnyEnemyAttack (be a)

instance RunMessage MrGrey where
  runMessage msg e@(MrGrey attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawEncounterCards iid (attrs.ability 1) 1
      pure e
    _ -> MrGrey <$> liftRunMessage msg attrs
