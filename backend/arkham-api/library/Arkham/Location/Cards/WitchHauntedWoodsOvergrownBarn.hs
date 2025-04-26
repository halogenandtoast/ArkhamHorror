module Arkham.Location.Cards.WitchHauntedWoodsOvergrownBarn (witchHauntedWoodsOvergrownBarn) where

import Arkham.Ability
import Arkham.Attack
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Spawn
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype WitchHauntedWoodsOvergrownBarn = WitchHauntedWoodsOvergrownBarn LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

witchHauntedWoodsOvergrownBarn :: LocationCard WitchHauntedWoodsOvergrownBarn
witchHauntedWoodsOvergrownBarn =
  location
    WitchHauntedWoodsOvergrownBarn
    Cards.witchHauntedWoodsOvergrownBarn
    3
    (PerPlayer 1)

instance HasAbilities WitchHauntedWoodsOvergrownBarn where
  getAbilities (WitchHauntedWoodsOvergrownBarn a) =
    extendRevealed1 a
      $ restricted a 1 Here
      $ freeReaction
      $ EnemyWouldSpawnAt AnyEnemy (not_ (be a) <> "Witch-Haunted Woods")

instance RunMessage WitchHauntedWoodsOvergrownBarn where
  runMessage msg l@(WitchHauntedWoodsOvergrownBarn attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 [(windowType -> Window.EnemyWouldSpawnAt enemyId _)] _ ->
      do
        insteadOfMatchingWith
          \case
            When (EnemySpawn details) -> details.enemy == enemyId
            _ -> False
          \case
            When (EnemySpawn details) ->
              pure [When $ EnemySpawn $ details {spawnDetailsSpawnAt = SpawnAtLocation attrs.id}]
            _ -> error "bad match"
        insteadOfMatchingWith
          \case
            EnemySpawn details -> details.enemy == enemyId
            _ -> False
          \case
            EnemySpawn details -> pure [EnemySpawn $ details {spawnDetailsSpawnAt = SpawnAtLocation attrs.id}]
            _ -> error "bad match"
        iids <- select $ investigatorAt $ toId attrs
        insteadOfMatchingWith
          \case
            After (EnemySpawn details) -> details.enemy == enemyId
            _ -> False
          \case
            After (EnemySpawn details) ->
              pure
                $ After (EnemySpawn $ details {spawnDetailsSpawnAt = SpawnAtLocation attrs.id})
                : map (InitiateEnemyAttack . enemyAttack details.enemy attrs) iids
            _ -> error "bad match"
        pure l
    _ -> WitchHauntedWoodsOvergrownBarn <$> liftRunMessage msg attrs
