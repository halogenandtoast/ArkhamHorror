module Arkham.Treachery.Cards.StalkedInTheDark (stalkedInTheDark) where

import Arkham.Matcher
import Arkham.Scenarios.TheMiskatonicMuseum.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype StalkedInTheDark = StalkedInTheDark TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stalkedInTheDark :: TreacheryCard StalkedInTheDark
stalkedInTheDark = treachery StalkedInTheDark Cards.stalkedInTheDark

instance RunMessage StalkedInTheDark where
  runMessage msg t@(StalkedInTheDark attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      getInPlayHuntingHorror >>= \case
        Just eid -> do
          readyThis eid
          enemyEngageInvestigator eid iid
          iids <- select $ colocatedWith iid
          for_ iids (initiateEnemyAttack eid attrs)
        Nothing -> gainSurge attrs
      pure t
    _ -> StalkedInTheDark <$> liftRunMessage msg attrs
