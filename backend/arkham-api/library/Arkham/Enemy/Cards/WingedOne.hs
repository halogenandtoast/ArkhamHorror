module Arkham.Enemy.Cards.WingedOne (wingedOne) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Window hiding (FlipLocation)
import Arkham.Window qualified as Window

newtype WingedOne = WingedOne EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wingedOne :: EnemyCard WingedOne
wingedOne = enemyWith WingedOne Cards.wingedOne (3, Static 3, 4) (3, 1) (spawnAtL ?~ "Bleak Plains")

instance HasAbilities WingedOne where
  getAbilities (WingedOne a) =
    extend1 a
      $ restricted a 1 (exists $ be a <> ReadyEnemy <> UnengagedEnemy)
      $ forced
      $ FlipLocation #when Anyone Anywhere

instance RunMessage WingedOne where
  runMessage msg e@(WingedOne attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 [windowType -> Window.FlipLocation _ lid] _ -> do
      moveTowards (attrs.ability 1) attrs lid
      pure e
    _ -> WingedOne <$> liftRunMessage msg attrs
