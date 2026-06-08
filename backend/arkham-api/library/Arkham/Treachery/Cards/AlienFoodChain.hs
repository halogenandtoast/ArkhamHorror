module Arkham.Treachery.Cards.AlienFoodChain (alienFoodChain) where

import Arkham.Enemy.Types (Field (EnemyRemainingHealth))
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Placement
import Arkham.Trait (Trait (Manifold))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype AlienFoodChain = AlienFoodChain TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alienFoodChain :: TreacheryCard AlienFoodChain
alienFoodChain = treachery AlienFoodChain Cards.alienFoodChain

instance HasModifiersFor AlienFoodChain where
  getModifiersFor (AlienFoodChain attrs) = case attrs.placement of
    AttachedToEnemy eid ->
      modified_ attrs eid [EnemyFight 1, EnemyEvade 1, AddKeyword Keyword.Hunter]
    _ -> pure mempty

instance RunMessage AlienFoodChain where
  runMessage msg t@(AlienFoodChain attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      manifolds <- selectWithField EnemyRemainingHealth $ EnemyWithTrait Manifold
      let withHealth = sortOn snd [(eid, h) | (eid, Just h) <- manifolds]
      case withHealth of
        [] -> gainSurge attrs
        ((eid, _) : _) -> do
          attachTreachery attrs eid
          healAllDamageAndHorror attrs eid
      pure t
    _ -> AlienFoodChain <$> liftRunMessage msg attrs
