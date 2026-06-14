module Arkham.Enemy.Cards.RandallTillinghast (randallTillinghast) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype RandallTillinghast = RandallTillinghast EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

randallTillinghast :: EnemyCard RandallTillinghast
randallTillinghast = enemy RandallTillinghast Cards.randallTillinghast (1, Static 1, 3) (1, 1)

-- Keywords (Relentless, Retaliate) are defined on the card def.
instance HasModifiersFor RandallTillinghast where
  getModifiersFor (RandallTillinghast a) = do
    -- Number of cards beneath the Tillinghast Esoterica location.
    cardsBeneath <-
      selectOne (locationIs Locations.tillinghastEsoterica) >>= \case
        Nothing -> pure 0
        Just lid -> length <$> field LocationCardsUnderneath lid
    health <- perPlayer cardsBeneath
    modifySelf a
      $ [CannotMove] <> [EnemyFight cardsBeneath | cardsBeneath > 0] <> [HealthModifier health | health > 0]

instance RunMessage RandallTillinghast where
  runMessage msg (RandallTillinghast attrs) = runQueueT $ RandallTillinghast <$> liftRunMessage msg attrs
