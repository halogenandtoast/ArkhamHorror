module Arkham.Enemy.Cards.AnetteMason (
  anetteMason,
  AnetteMason (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Phase
import Arkham.Projection
import Arkham.Timing qualified as Timing

newtype AnetteMason = AnetteMason EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

anetteMason :: EnemyCard AnetteMason
anetteMason = enemy AnetteMason Cards.anetteMason (4, PerPlayer 4, 4) (1, 1)

-- Forced - After the enemy phase begins: Discard the top 3 cards of the
-- encounter deck. Spawn each Witch enemy discarded by this effect at Anette
-- Mason's location. If no Witch enemies are discarded by this effect, ready
-- her.

instance HasAbilities AnetteMason where
  getAbilities (AnetteMason a) =
    withBaseAbilities
      a
      [ mkAbility a 1 $
          ForcedAbility $
            PhaseBegins Timing.After $
              PhaseIs
                EnemyPhase
      ]

instance RunMessage AnetteMason where
  runMessage msg e@(AnetteMason attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ DiscardTopOfEncounterDeck iid 3 (toSource attrs) (Just $ toTarget attrs)
      pure e
    DiscardedTopOfEncounterDeck _ cards _ (isTarget attrs -> True) -> do
      mLocation <- field EnemyLocation (toId attrs)
      for_ mLocation $ \location -> do
        for cards $ \card -> do
          pushM $ createEnemyAt_ (EncounterCard card) location Nothing
      when (null cards) $ push $ Ready (toTarget attrs)
      pure e
    _ -> AnetteMason <$> runMessage msg attrs
