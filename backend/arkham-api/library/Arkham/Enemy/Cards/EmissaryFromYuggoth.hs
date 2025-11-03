module Arkham.Enemy.Cards.EmissaryFromYuggoth (emissaryFromYuggoth) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (DiscoverClues)
import Arkham.Matcher

newtype EmissaryFromYuggoth = EmissaryFromYuggoth EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

emissaryFromYuggoth :: EnemyCard EmissaryFromYuggoth
emissaryFromYuggoth = enemy EmissaryFromYuggoth Cards.emissaryFromYuggoth (3, Static 4, 3) (1, 1)

instance HasAbilities EmissaryFromYuggoth where
  getAbilities (EmissaryFromYuggoth a) =
    extend1 a $ mkAbility a 1 $ forced $ DiscoverClues #after You LocationWithConcealedCard (atLeast 1)

instance RunMessage EmissaryFromYuggoth where
  runMessage msg e@(EmissaryFromYuggoth attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    _ -> EmissaryFromYuggoth <$> liftRunMessage msg attrs
