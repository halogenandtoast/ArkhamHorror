module Arkham.Enemy.Cards.TemporalDevourer (
  temporalDevourer,
  TemporalDevourer (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Timing qualified as Timing
import Arkham.Trait (Trait (Extradimensional, Shattered))

newtype TemporalDevourer = TemporalDevourer EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

temporalDevourer :: EnemyCard TemporalDevourer
temporalDevourer =
  enemyWith
    TemporalDevourer
    Cards.temporalDevourer
    (4, Static 5, 4)
    (1, 1)
    $ ( spawnAtL
          ?~ SpawnLocation
            ( FarthestLocationFromYou
                (LocationMatchAny $ map LocationWithTrait [Shattered, Extradimensional])
            )
      )
      . (surgeIfUnableToSpawnL .~ True)

instance HasAbilities TemporalDevourer where
  getAbilities (TemporalDevourer a) =
    withBaseAbilities
      a
      [ mkAbility a 1
          $ ForcedAbility
          $ EnemyEnters
            Timing.After
            ( LocationMatchAny $
                map LocationWithTrait [Shattered, Extradimensional]
            )
          $ EnemyWithId
          $ toId a
      ]

instance RunMessage TemporalDevourer where
  runMessage msg e@(TemporalDevourer attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      mlid <- field EnemyLocation (toId e)
      for_ mlid $ \lid -> do
        push $ PlaceClues (toAbilitySource attrs 1) (toTarget lid) 1
      pure e
    _ -> TemporalDevourer <$> runMessage msg attrs
