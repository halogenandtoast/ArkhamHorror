module Arkham.Enemy.Cards.TheThingThatFollows (
  theThingThatFollows,
  TheThingThatFollows (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype TheThingThatFollows = TheThingThatFollows EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

theThingThatFollows :: EnemyCard TheThingThatFollows
theThingThatFollows =
  enemyWith
    TheThingThatFollows
    Cards.theThingThatFollows
    (3, Static 2, 3)
    (1, 1)
    $ (spawnAtL ?~ SpawnAt (FarthestLocationFromYou Anywhere))
    . (\a -> a & preyL .~ BearerOf (toId a))

instance HasAbilities TheThingThatFollows where
  getAbilities (TheThingThatFollows x) =
    withBaseAbilities
      x
      [ mkAbility x 1
          $ ForcedAbility
          $ EnemyWouldBeDefeated Timing.When
          $ EnemyWithId
          $ toId x
      ]

instance RunMessage TheThingThatFollows where
  runMessage msg e@(TheThingThatFollows attrs) = case msg of
    UseCardAbility iid source 1 _ _
      | isSource attrs source ->
          e
            <$ pushAll
              [ CancelNext (toSource attrs) EnemyDefeatedMessage
              , ShuffleIntoDeck (Deck.InvestigatorDeck iid) $ toTarget attrs
              ]
    _ -> TheThingThatFollows <$> runMessage msg attrs
