module Arkham.Enemy.Cards.AnetteMason (anetteMason) where

import Arkham.Ability
import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Enemy.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Trait (Trait (Witch))

newtype AnetteMason = AnetteMason EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

anetteMason :: EnemyCard AnetteMason
anetteMason = enemy AnetteMason Cards.anetteMason (4, PerPlayer 4, 4) (1, 1)

instance HasAbilities AnetteMason where
  getAbilities (AnetteMason a) = extend1 a $ mkAbility a 1 $ forced $ PhaseBegins #after #enemy

instance RunMessage AnetteMason where
  runMessage msg e@(AnetteMason attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discardTopOfEncounterDeckAndHandle iid attrs 3 attrs
      pure e
    DiscardedTopOfEncounterDeck _ cards _ (isTarget attrs -> True) -> do
      let witches = filterCards (CardWithTrait Witch) $ map toCard cards
      if null witches
        then readyThis attrs
        else
          field EnemyLocation (toId attrs) >>= traverse_ \location -> do
            for_ witches (`createEnemyAt_` location)
      pure e
    _ -> AnetteMason <$> liftRunMessage msg attrs
