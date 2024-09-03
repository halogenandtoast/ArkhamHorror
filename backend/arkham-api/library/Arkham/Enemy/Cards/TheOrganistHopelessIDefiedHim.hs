module Arkham.Enemy.Cards.TheOrganistHopelessIDefiedHim (theOrganistHopelessIDefiedHim, TheOrganistHopelessIDefiedHim (..)) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype TheOrganistHopelessIDefiedHim = TheOrganistHopelessIDefiedHim EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor TheOrganistHopelessIDefiedHim where
  getModifiersFor target (TheOrganistHopelessIDefiedHim attrs) | isTarget attrs target = do
    pure $ toModifiers attrs [CannotBeDamaged]
  getModifiersFor _ _ = pure []

instance HasAbilities TheOrganistHopelessIDefiedHim where
  getAbilities (TheOrganistHopelessIDefiedHim attrs) =
    withBaseAbilities
      attrs
      [groupLimit PerRound $ mkAbility attrs 1 $ forced $ MovedFromHunter #after (be attrs)]

theOrganistHopelessIDefiedHim :: EnemyCard TheOrganistHopelessIDefiedHim
theOrganistHopelessIDefiedHim =
  enemyWith
    TheOrganistHopelessIDefiedHim
    Cards.theOrganistHopelessIDefiedHim
    (5, Static 1, 3)
    (0, 3)
    (healthL .~ Nothing)

instance RunMessage TheOrganistHopelessIDefiedHim where
  runMessage msg e@(TheOrganistHopelessIDefiedHim attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      isEngaged <- selectAny $ investigatorEngagedWith attrs
      unless isEngaged
        $ pushAll
          [ roundModifier source attrs CannotAttack
          , HunterMove (toId attrs)
          ]
      pure e
    _ -> TheOrganistHopelessIDefiedHim <$> runMessage msg attrs
