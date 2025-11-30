module Arkham.Enemy.Cards.TheClaretKnightCoterieKingpin (theClaretKnightCoterieKingpin) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype TheClaretKnightCoterieKingpin = TheClaretKnightCoterieKingpin EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theClaretKnightCoterieKingpin :: EnemyCard TheClaretKnightCoterieKingpin
theClaretKnightCoterieKingpin = enemy TheClaretKnightCoterieKingpin Cards.theClaretKnightCoterieKingpin (0, Static 1, 0) (0, 0)

instance RunMessage TheClaretKnightCoterieKingpin where
  runMessage msg (TheClaretKnightCoterieKingpin attrs) = runQueueT $ case msg of
    _ -> TheClaretKnightCoterieKingpin <$> liftRunMessage msg attrs
