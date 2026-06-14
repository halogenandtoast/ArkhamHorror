module Arkham.Enemy.Cards.VaultAttendant (vaultAttendant) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher
import Arkham.Scenarios.TheGrandVault.Helpers

newtype VaultAttendant = VaultAttendant EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vaultAttendant :: EnemyCard VaultAttendant
vaultAttendant =
  enemyWith VaultAttendant Cards.vaultAttendant (2, Static 2, 4) (2, 0)
    $ (preyL .~ Prey MostClues)
    . (spawnAtL ?~ SpawnEngagedWith MostClues)

-- | "When Vault Attendant moves via its hunter keyword, its location is
-- considered connected to each activated location." HunterConnectedTo is only
-- consulted while resolving hunter movement, so applying one per activated
-- location is sufficient.
instance HasModifiersFor VaultAttendant where
  getModifiersFor (VaultAttendant a) = do
    activated <- select activatedLocation
    modifySelf a $ map HunterConnectedTo activated

instance RunMessage VaultAttendant where
  runMessage msg (VaultAttendant attrs) = runQueueT $ VaultAttendant <$> liftRunMessage msg attrs
