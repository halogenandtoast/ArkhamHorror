module Arkham.Types.Enemy.Cards.SwiftByakhee
  ( swiftByakhee
  , SwiftByakhee(..)
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Effect.Window
import Arkham.Types.EffectMetadata
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Prey
import Arkham.Types.Target
import Arkham.Types.Timing qualified as Timing

newtype SwiftByakhee = SwiftByakhee EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

swiftByakhee :: EnemyCard SwiftByakhee
swiftByakhee = enemyWith
  SwiftByakhee
  Cards.swiftByakhee
  (2, Static 3, 2)
  (1, 1)
  (preyL .~ LowestRemainingSanity)

instance HasAbilities SwiftByakhee where
  getAbilities (SwiftByakhee a) = withBaseAbilities
    a
    [ mkAbility a 1
      $ ForcedAbility
      $ MovedFromHunter Timing.When
      $ EnemyWithId
      $ toId a
    ]

instance EnemyRunner env => RunMessage env SwiftByakhee where
  runMessage msg e@(SwiftByakhee attrs) = case msg of
    UseCardAbility iid source windows' 1 payment | isSource attrs source -> do
      closestPathIds <- map unClosestPathLocationId
        <$> getSetList (enemyLocation attrs, enemyPrey attrs)
      push $ chooseOrRunOne
        iid
        [ TargetLabel
            (LocationTarget pathId)
            [ EnemyMove (toId attrs) (enemyLocation attrs) pathId
            , UseCardAbilityChoice iid source windows' 1 payment $ IntMetadata 0
            ]
        | pathId <- closestPathIds
        ]
      pure e
    UseCardAbilityChoice iid source windows' 1 payment (IntMetadata n)
      | isSource attrs source -> do
        preyIds <- mapSet unPreyId <$> getSet (enemyPrey attrs)
        when (null $ enemyEngagedInvestigators attrs `intersect` preyIds) $ do
          closestPathIds <- map unClosestPathLocationId
            <$> getSetList (enemyLocation attrs, enemyPrey attrs)
          pushAll
            $ [ CreateWindowModifierEffect
                  EffectPhaseWindow
                  (EffectModifiers $ toModifiers attrs [CannotAttack])
                  source
                  (toTarget attrs)
              | n == 0
              ]
            <> [ chooseOrRunOne
                   iid
                   [ TargetLabel
                       (LocationTarget pathId)
                       [ EnemyMove (toId attrs) (enemyLocation attrs) pathId
                       , UseCardAbilityChoice iid source windows' 1 payment
                         $ IntMetadata (n + 1)
                       ]
                   | pathId <- closestPathIds
                   ]
               ]
        pure e
    _ -> SwiftByakhee <$> runMessage msg attrs
