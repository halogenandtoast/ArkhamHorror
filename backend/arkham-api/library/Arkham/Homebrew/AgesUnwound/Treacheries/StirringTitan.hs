module Arkham.Homebrew.AgesUnwound.Treacheries.StirringTitan (stirringTitan) where

import Arkham.GameValue
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted

newtype StirringTitan = StirringTitan TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | Surge is on the card def.
stirringTitan :: TreacheryCard StirringTitan
stirringTitan = treachery StirringTitan Cards.stirringTitan

{- | "Revelation - If Hound of Unmaking is in play and undamaged, deal 1 damage
to it."
-}
instance RunMessage StirringTitan where
  runMessage msg t@(StirringTitan attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      hounds <- select $ enemyIs Enemies.houndOfUnmaking <> EnemyWithDamage (EqualTo $ Static 0)
      for_ hounds $ nonAttackEnemyDamage Nothing attrs 1
      pure t
    _ -> StirringTitan <$> liftRunMessage msg attrs
