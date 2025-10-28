module Arkham.Treachery.Cards.SecretsOfTheBeyond (secretsOfTheBeyond) where

import Arkham.Enemy.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenarios.InTheClutchesOfChaos.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SecretsOfTheBeyond = SecretsOfTheBeyond TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secretsOfTheBeyond :: TreacheryCard SecretsOfTheBeyond
secretsOfTheBeyond = treachery SecretsOfTheBeyond Cards.secretsOfTheBeyond

instance RunMessage SecretsOfTheBeyond where
  runMessage msg t@(SecretsOfTheBeyond attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      enemies <- selectWithField EnemyLocation $ EnemyWithMostDoom $ EnemyWithAnyDoom <> #cultist
      case nonEmpty enemies of
        Just ((enemyId, _) :| _) -> do
          maxDoom <- field EnemyDoom enemyId
          chooseOrRunOneM iid do
            for_ enemies \(enemy, mlocation) -> for_ mlocation \location ->
              targeting enemy $ placeBreaches location maxDoom
        Nothing -> gainSurge attrs
      pure t
    _ -> SecretsOfTheBeyond <$> liftRunMessage msg attrs
