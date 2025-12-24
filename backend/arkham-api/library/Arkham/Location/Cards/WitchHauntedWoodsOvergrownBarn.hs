module Arkham.Location.Cards.WitchHauntedWoodsOvergrownBarn (witchHauntedWoodsOvergrownBarn) where

import Arkham.Ability
import Arkham.Attack
import Arkham.GameValue
import Arkham.Helpers.Window
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Spawn

newtype WitchHauntedWoodsOvergrownBarn = WitchHauntedWoodsOvergrownBarn LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

witchHauntedWoodsOvergrownBarn :: LocationCard WitchHauntedWoodsOvergrownBarn
witchHauntedWoodsOvergrownBarn =
  location WitchHauntedWoodsOvergrownBarn Cards.witchHauntedWoodsOvergrownBarn 3 (PerPlayer 1)

instance HasAbilities WitchHauntedWoodsOvergrownBarn where
  getAbilities (WitchHauntedWoodsOvergrownBarn a) =
    extendRevealed1 a
      $ groupLimit PerWindow
      $ restricted a 1 Here
      $ freeReaction
      $ EnemyWouldSpawnAt AnyEnemy (not_ (be a) <> "Witch-Haunted Woods")

instance RunMessage WitchHauntedWoodsOvergrownBarn where
  runMessage msg l@(WitchHauntedWoodsOvergrownBarn attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getEnemy -> enemy) _ -> do
      iids <- select $ investigatorAt $ toId attrs
      updateSpawnDetails enemy \details ->
        details
          { spawnDetailsSpawnAt = SpawnAtLocation attrs.id
          , spawnDetailsAfter =
              EnemyCheckEngagement enemy : map (InitiateEnemyAttack . enemyAttack details.enemy attrs) iids
          }
      pure l
    _ -> WitchHauntedWoodsOvergrownBarn <$> liftRunMessage msg attrs
