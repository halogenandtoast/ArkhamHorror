module Arkham.Event.Events.SpiritualCharm (spiritualCharm) where

import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Matcher
import Arkham.Projection

newtype SpiritualCharm = SpiritualCharm EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spiritualCharm :: EventCard SpiritualCharm
spiritualCharm = event SpiritualCharm Cards.spiritualCharm

instance RunMessage SpiritualCharm where
  runMessage msg e@(SpiritualCharm attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      withLocationOf iid \lid -> do
        let m = LocationWithId lid
        enemies <- select $ NonEliteEnemy <> at_ (oneOf [m, connectedFrom m])
        chooseTargetM iid enemies \enemy -> do
          push $ EnemyMove enemy lid
          push $ EnemyEngageInvestigator enemy iid
          damage <- field EnemyHealthDamage enemy
          horror <- field EnemySanityDamage enemy
          gainResources iid attrs (damage + horror)
      pure e
    _ -> SpiritualCharm <$> liftRunMessage msg attrs
