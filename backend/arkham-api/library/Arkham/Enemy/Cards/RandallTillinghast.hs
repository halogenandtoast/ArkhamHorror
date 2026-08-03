module Arkham.Enemy.Cards.RandallTillinghast (randallTillinghast) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (LocationCardsUnderneath))
import Arkham.Matcher
import Arkham.Projection

newtype RandallTillinghast = RandallTillinghast EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

randallTillinghast :: EnemyCard RandallTillinghast
randallTillinghast = enemy RandallTillinghast Cards.randallTillinghast

{- | "Randall Tillinghast cannot move and gets +1 fight and +1[per_investigator]
health for each card beneath Tillinghast Esoterica." The stack shrinks as the
investigators buy their way through it, so he weakens as they take the artifacts.
Relentless and Retaliate are printed on the card def.
-}
instance HasModifiersFor RandallTillinghast where
  getModifiersFor (RandallTillinghast a) = do
    cardsBeneath <-
      selectOne (locationIs Locations.tillinghastEsotericaEphemeralShop)
        >>= maybe (pure 0) (fieldMap LocationCardsUnderneath length)
    health <- perPlayer cardsBeneath
    modifySelf a
      $ [CannotMove]
      <> [EnemyFight cardsBeneath | cardsBeneath > 0]
      <> [HealthModifier health | health > 0]

instance RunMessage RandallTillinghast where
  runMessage msg (RandallTillinghast attrs) = runQueueT $ RandallTillinghast <$> liftRunMessage msg attrs
