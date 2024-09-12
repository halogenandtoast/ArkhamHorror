module Arkham.Enemy.Cards.AnetteMason (anetteMason, AnetteMason (..)) where

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
  getAbilities (AnetteMason a) = withBaseAbilities a [mkAbility a 1 $ ForcedAbility $ PhaseBegins #after #enemy]

instance RunMessage AnetteMason where
  runMessage msg e@(AnetteMason attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ DiscardTopOfEncounterDeck iid 3 (toSource attrs) (Just $ toTarget attrs)
      pure e
    DiscardedTopOfEncounterDeck _ cards _ (isTarget attrs -> True) -> do
      let witches = filterCards (CardWithTrait Witch) $ map toCard cards
      if null witches
        then push $ Ready (toTarget attrs)
        else do
          mLocation <- field EnemyLocation (toId attrs)
          for_ mLocation \location -> do
            for_ witches \card -> createEnemyAt_ card location
      pure e
    _ -> AnetteMason <$> liftRunMessage msg attrs
