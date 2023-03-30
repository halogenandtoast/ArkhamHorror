module Arkham.Enemy.Cards.TheOrganistHopelessIDefiedHim
  ( theOrganistHopelessIDefiedHim
  , TheOrganistHopelessIDefiedHim(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing

newtype TheOrganistHopelessIDefiedHim = TheOrganistHopelessIDefiedHim EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor TheOrganistHopelessIDefiedHim where
  getModifiersFor target (TheOrganistHopelessIDefiedHim attrs)
    | isTarget attrs target = pure $ toModifiers attrs [CannotBeDamaged]
  getModifiersFor _ _ = pure []

instance HasAbilities TheOrganistHopelessIDefiedHim where
  getAbilities (TheOrganistHopelessIDefiedHim attrs) = withBaseAbilities
    attrs
    [ limitedAbility (GroupLimit PerRound 1)
      $ mkAbility attrs 1
      $ ForcedAbility
      $ MovedFromHunter Timing.After
      $ EnemyWithId
      $ toId attrs
    ]

theOrganistHopelessIDefiedHim :: EnemyCard TheOrganistHopelessIDefiedHim
theOrganistHopelessIDefiedHim = enemy
  TheOrganistHopelessIDefiedHim
  Cards.theOrganistHopelessIDefiedHim
  (5, Static 1, 3)
  (0, 3)

instance RunMessage TheOrganistHopelessIDefiedHim where
  runMessage msg e@(TheOrganistHopelessIDefiedHim attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      isEngaged <- selectAny
        $ InvestigatorEngagedWith (EnemyWithId $ toId attrs)
      unless isEngaged $ pushAll
        [ CreateEffect (toCardCode attrs) Nothing source (toTarget attrs)
        , HunterMove (toId attrs)
        ]
      pure e
    _ -> TheOrganistHopelessIDefiedHim <$> runMessage msg attrs
