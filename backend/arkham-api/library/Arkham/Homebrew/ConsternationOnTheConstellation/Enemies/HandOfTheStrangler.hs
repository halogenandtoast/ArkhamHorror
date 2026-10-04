module Arkham.Homebrew.ConsternationOnTheConstellation.Enemies.HandOfTheStrangler (handOfTheStrangler) where

import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Enemies qualified as Cards

newtype HandOfTheStrangler = HandOfTheStrangler EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

{- | The back of a Crate of Goods: a severed hand that cannot be defeated or evaded
and prints no fight, health or evade. A Fight action moves it to a connecting
location or attaches it to a non-Elite enemy, and it deals 1 damage to whatever
it is attached to at the end of each enemy phase.
-}

-- TODO: the printed text is not implemented yet. The full transcription is in
-- @docs/homebrew/data/consternation-on-the-constellation-card-text.md@.
handOfTheStrangler :: EnemyCard HandOfTheStrangler
handOfTheStrangler = enemy HandOfTheStrangler Cards.handOfTheStrangler

instance RunMessage HandOfTheStrangler where
  runMessage msg (HandOfTheStrangler attrs) = runQueueT $ HandOfTheStrangler <$> liftRunMessage msg attrs
