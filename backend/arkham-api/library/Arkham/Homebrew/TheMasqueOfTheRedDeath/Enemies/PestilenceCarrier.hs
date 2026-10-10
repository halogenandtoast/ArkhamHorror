module Arkham.Homebrew.TheMasqueOfTheRedDeath.Enemies.PestilenceCarrier (pestilenceCarrier) where

import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.TheMasqueOfTheRedDeath.Helpers (describedSkullEffect)
import Arkham.Matcher
import Arkham.Message.Lifted.Move

newtype PestilenceCarrier = PestilenceCarrier EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pestilenceCarrier :: EnemyCard PestilenceCarrier
pestilenceCarrier =
  enemyWith PestilenceCarrier Cards.pestilenceCarrier
    $ spawnAtL
    ?~ SpawnAt (locationIs Locations.blackChamber)

instance HasAbilities PestilenceCarrier where
  -- "While Pestilence Carrier is ready and unengaged, each location gains:
  -- '[skull]: Move Pestilence Carrier once toward you.'"
  getAbilities (PestilenceCarrier a) =
    extend
      a
      [ describedSkullEffect 0 "Move Pestilence Carrier once toward you." (proxied Anywhere a) 1
      | a.ready
      , isNothing a.placement.inThreatAreaOf
      ]

instance RunMessage PestilenceCarrier where
  runMessage msg e@(PestilenceCarrier attrs) = runQueueT $ case msg of
    UseThisAbility iid (isProxySource attrs -> True) 1 -> do
      moveTowardsMatching (attrs.ability 1) attrs (locationWithInvestigator iid)
      pure e
    _ -> PestilenceCarrier <$> liftRunMessage msg attrs
