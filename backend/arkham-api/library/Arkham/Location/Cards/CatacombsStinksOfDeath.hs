module Arkham.Location.Cards.CatacombsStinksOfDeath (catacombsStinksOfDeath) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Enemy (disengageEnemyFromAll)
import Arkham.Helpers.Window (defeatedEnemy)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.FilmFatale.Helpers

newtype CatacombsStinksOfDeath = CatacombsStinksOfDeath LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

catacombsStinksOfDeath :: LocationCard CatacombsStinksOfDeath
catacombsStinksOfDeath = location CatacombsStinksOfDeath Cards.catacombsStinksOfDeath 2 (PerPlayer 1)

instance HasAbilities CatacombsStinksOfDeath where
  getAbilities (CatacombsStinksOfDeath a) =
    extend a
      $ if a.revealed
        then
          [ scenarioI18n $ hauntedI "catacombs.haunted" a 2
          , restricted a 3 Here $ forced $ EnemyWouldBeDefeated #when (at_ (be a))
          ]
        else
          [ restricted a 1 (exists $ enemyIs Enemies.theContessaNeedlesslySmug <> EnemyCanMove)
              $ forced
              $ UnrevealedRevealLocation #when You (be a)
          ]

instance RunMessage CatacombsStinksOfDeath where
  runMessage msg l@(CatacombsStinksOfDeath attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #combat (Fixed 3)
      pure l
    FailedThisSkillTest _iid (isAbilitySource attrs 1 -> True) -> do
      moveContessa (attrs.ability 1) attrs
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      enemies <- select $ NearestEnemyTo iid EnemyCanMove
      chooseTargetM iid enemies \enemy -> do
        readyThis enemy
        sendMessage enemy HuntersMove
        sendMessage enemy EnemiesAttack
      pure l
    UseCardAbility _iid (isSource attrs -> True) 3 (defeatedEnemy -> enemy) _ -> do
      cancelEnemyDefeat enemy
      healAllDamage (attrs.ability 3) enemy
      disengageEnemyFromAll enemy
      exhaustThis enemy
      pure l
    _ -> CatacombsStinksOfDeath <$> liftRunMessage msg attrs
