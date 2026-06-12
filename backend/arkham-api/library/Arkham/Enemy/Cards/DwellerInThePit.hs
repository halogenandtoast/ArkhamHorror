module Arkham.Enemy.Cards.DwellerInThePit (dwellerInThePit) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers (getVengeanceInVictoryDisplay)
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Scenarios.RelicsOfThePast.Helpers
import Arkham.Spawn

newtype DwellerInThePit = DwellerInThePit EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dwellerInThePit :: EnemyCard DwellerInThePit
dwellerInThePit =
  enemyWith DwellerInThePit Cards.dwellerInThePit (3, Static 6, 3) (2, 1)
    $ spawnAtL
    ?~ SpawnEngagedWith (InvestigatorWithTitle "Monterey Jack")

instance HasModifiersFor DwellerInThePit where
  getModifiersFor (DwellerInThePit a) = do
    vengeance <- getVengeanceInVictoryDisplay
    healthBonus <- perPlayer vengeance
    modifySelfWhen
      a
      (vengeance > 0)
      [HealthModifier healthBonus, EnemyFight (min 3 vengeance)]

instance HasAbilities DwellerInThePit where
  getAbilities (DwellerInThePit a) =
    extend1 a
      $ restricted a 1 (youExist $ not_ (InvestigatorWithSupply Satchel))
      $ forced
      $ DealtDamage #after (SourceIsEnemyAttack $ be a) You

instance RunMessage DwellerInThePit where
  runMessage msg e@(DwellerInThePit attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      shuffleAncientAssetsIntoExplorationDeck iid
      pure e
    _ -> DwellerInThePit <$> liftRunMessage msg attrs
