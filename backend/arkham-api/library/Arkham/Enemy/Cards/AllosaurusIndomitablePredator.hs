module Arkham.Enemy.Cards.AllosaurusIndomitablePredator (allosaurusIndomitablePredator) where

import Arkham.Ability
import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher
import Arkham.Message (ReplaceStrategy (..))
import Arkham.Token

newtype AllosaurusIndomitablePredator = AllosaurusIndomitablePredator EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

allosaurusIndomitablePredator :: EnemyCard AllosaurusIndomitablePredator
allosaurusIndomitablePredator =
  enemyWith
    AllosaurusIndomitablePredator
    Cards.allosaurusIndomitablePredator
    (5, Static 1, 5)
    (3, 0)
    (healthL .~ Nothing)

instance HasModifiersFor AllosaurusIndomitablePredator where
  getModifiersFor (AllosaurusIndomitablePredator a) = do
    modifySelf a [CannotBeDefeated, CannotBeDamaged]

instance HasAbilities AllosaurusIndomitablePredator where
  getAbilities (AllosaurusIndomitablePredator a) =
    extend1 a
      $ restricted a 1 (thisExists a (EnemyWithTokens 4 Seal))
      $ forced
      $ PlacedToken #after AnySource (targetIs a) Seal

instance RunMessage AllosaurusIndomitablePredator where
  runMessage msg e@(AllosaurusIndomitablePredator attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      flipOverBy iid (attrs.ability 1) attrs
      pure e
    Flip _ _ (isTarget attrs -> True) -> do
      let rampaging = lookupCard Cards.allosaurusRampagingPredator attrs.cardId
      push $ ReplaceEnemy attrs.id rampaging Swap
      pure e
    _ -> AllosaurusIndomitablePredator <$> liftRunMessage msg attrs
