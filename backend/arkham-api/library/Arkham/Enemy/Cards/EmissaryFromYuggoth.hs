module Arkham.Enemy.Cards.EmissaryFromYuggoth (emissaryFromYuggoth) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype EmissaryFromYuggoth = EmissaryFromYuggoth EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

emissaryFromYuggoth :: EnemyCard EmissaryFromYuggoth
emissaryFromYuggoth = enemy EmissaryFromYuggoth Cards.emissaryFromYuggoth (3, Static 4, 3) (1, 1)

instance RunMessage EmissaryFromYuggoth where
  runMessage msg (EmissaryFromYuggoth attrs) = runQueueT $ case msg of
    _ -> EmissaryFromYuggoth <$> liftRunMessage msg attrs
