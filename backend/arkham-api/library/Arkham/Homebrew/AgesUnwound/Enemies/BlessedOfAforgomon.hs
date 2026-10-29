module Arkham.Homebrew.AgesUnwound.Enemies.BlessedOfAforgomon (blessedOfAforgomon) where

import Arkham.Ability
import Arkham.Classes.HasQueue (overMessagesM)
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (
  ModifierType (CanBeFoughtAsIfAtYourLocation, CannotBeEvaded, CannotSpendLastActions),
  modifySelect,
  modifySelf,
 )
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Cards
import Arkham.Matcher
import Arkham.Placement

newtype BlessedOfAforgomon = BlessedOfAforgomon EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blessedOfAforgomon :: EnemyCard BlessedOfAforgomon
blessedOfAforgomon = enemy BlessedOfAforgomon Cards.blessedOfAforgomon

instance HasAbilities BlessedOfAforgomon where
  getAbilities (BlessedOfAforgomon a) =
    map allowUnengagedEvade
      $ extend1 a
      $ mkAbility a 1
      $ forced
      $ EnemyWouldSpawnAt (be a) Anywhere
   where
    allowUnengagedEvade ability
      | abilityIs ability #evade =
          overrideAbilityCriteria
            (CriteriaOverride $ EnemyCriteria $ ThisEnemy $ EnemyWithEvade <> EnemyWithoutModifier CannotBeEvaded)
            ability
      | otherwise = ability

-- Fight and evade permissions do not move or engage the enemy.
instance HasModifiersFor BlessedOfAforgomon where
  getModifiersFor (BlessedOfAforgomon a) = do
    modifySelf a [CanBeFoughtAsIfAtYourLocation]
    unless a.exhausted $ modifySelect a Anyone [CannotSpendLastActions 1]

instance RunMessage BlessedOfAforgomon where
  runMessage msg e@(BlessedOfAforgomon attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      -- The first would-spawn window precedes EnemySpawn, so its details
      -- still live on the queue. The later window uses stored spawn details.
      lift $ overMessagesM $ pure . singleton . redirectSpawn
      updateSpawnDetails attrs.id \details ->
        details {spawnDetailsSpawnAt = SpawnPlaced NextToAgenda}
      pure e
    _ -> BlessedOfAforgomon <$> liftRunMessage msg attrs
   where
    redirectSpawn = \case
      EnemySpawn details
        | details.enemy == attrs.id ->
            EnemySpawn details {spawnDetailsSpawnAt = SpawnPlaced NextToAgenda}
      Do inner -> Do $ redirectSpawn inner
      When inner -> When $ redirectSpawn inner
      After inner -> After $ redirectSpawn inner
      Will inner -> Will $ redirectSpawn inner
      other -> other
