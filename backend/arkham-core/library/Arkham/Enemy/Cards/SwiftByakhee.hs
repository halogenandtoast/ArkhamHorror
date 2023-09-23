module Arkham.Enemy.Cards.SwiftByakhee (
  swiftByakhee,
  SwiftByakhee (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Distance
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Timing qualified as Timing

newtype SwiftByakhee = SwiftByakhee EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

swiftByakhee :: EnemyCard SwiftByakhee
swiftByakhee =
  enemyWith
    SwiftByakhee
    Cards.swiftByakhee
    (2, Static 3, 2)
    (1, 1)
    (preyL .~ Prey LowestRemainingSanity)

instance HasAbilities SwiftByakhee where
  getAbilities (SwiftByakhee a) =
    withBaseAbilities
      a
      [ mkAbility a 1
          $ ForcedAbility
          $ MovedFromHunter Timing.When
          $ EnemyWithId
          $ toId a
      ]

choosePrey :: EnemyAttrs -> (InvestigatorId, LocationId, Distance) -> UI Message
choosePrey attrs (iid, pathId, distance) =
  targetLabel iid
    $ [noAttack | unDistance distance <= 1]
    <> [MoveUntil pathId (toTarget attrs)]
 where
  noAttack =
    CreateWindowModifierEffect
      EffectPhaseWindow
      (EffectModifiers $ toModifiers attrs [CannotAttack])
      (toSource attrs)
      (toTarget attrs)

instance RunMessage SwiftByakhee where
  runMessage msg e@(SwiftByakhee attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      enemyLocation <- field EnemyLocation (toId attrs)
      for_ enemyLocation $ \loc -> do
        prey <- selectList (enemyPrey attrs)
        preyWithLocationsAndDistances <- fmap catMaybes $ for prey $ \preyId -> do
          mlid <- selectOne $ locationWithInvestigator preyId
          case mlid of
            Nothing -> pure Nothing
            Just lid -> do
              distance <- fromMaybe (Distance 1000) <$> getDistance loc lid
              pure $ Just (preyId, lid, distance)
        push $ chooseOrRunOne iid $ map (choosePrey attrs) preyWithLocationsAndDistances
      pure e
    _ -> SwiftByakhee <$> runMessage msg attrs
