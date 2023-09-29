module Arkham.Enemy.Cards.Basilisk (
  basilisk,
  Basilisk (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype Basilisk = Basilisk EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

basilisk :: EnemyCard Basilisk
basilisk =
  enemyWith Basilisk Cards.basilisk (4, Static 4, 4) (2, 0)
    $ spawnAtL
    ?~ SpawnAt (LocationWithDistanceFrom 1 $ locationIs Locations.mouthOfKnYanTheCavernsMaw)

instance HasAbilities Basilisk where
  getAbilities (Basilisk a) =
    withBaseAbilities
      a
      [ limitedAbility (PerCopyLimit Cards.basilisk PerRound 1)
          $ mkAbility a 1
          $ ForcedAbility
          $ PlacedCounterOnLocation
            Timing.After
            (LocationWithTitle "Mouth of K'n-yan")
            AnySource
            ResourceCounter
            AnyValue
      ]

instance RunMessage Basilisk where
  runMessage msg e@(Basilisk attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ ShuffleBackIntoEncounterDeck (toTarget attrs)
      pure e
    _ -> Basilisk <$> runMessage msg attrs
