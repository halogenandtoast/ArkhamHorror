module Arkham.Enemy.Cards.BlackChamberOperative (blackChamberOperative) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyEvaded)
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Investigator.Projection ()
import Arkham.Matcher

newtype BlackChamberOperative = BlackChamberOperative EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blackChamberOperative :: EnemyCard BlackChamberOperative
blackChamberOperative =
  enemyWith BlackChamberOperative Cards.blackChamberOperative (4, Static 2, 4) (1, 1) preyIsOnlyBearer

instance HasAbilities BlackChamberOperative where
  getAbilities (BlackChamberOperative a) = extend a [mkAbility a 1 $ forced $ EnemyEvaded #after You (be a)]

instance RunMessage BlackChamberOperative where
  runMessage msg e@(BlackChamberOperative attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      clues <- iid.clues
      miid <- getLocationOf iid
      if clues > 0 && isJust miid
        then placeCluesOnLocation iid (attrs.ability 1) 1
        else do
          readyThis attrs
          engageEnemy iid attrs
          initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    _ -> BlackChamberOperative <$> liftRunMessage msg attrs
