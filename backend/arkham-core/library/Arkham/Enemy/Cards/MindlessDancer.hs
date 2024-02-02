module Arkham.Enemy.Cards.MindlessDancer (
  mindlessDancer,
  MindlessDancer (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Window qualified as Window

newtype MindlessDancer = MindlessDancer EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

mindlessDancer :: EnemyCard MindlessDancer
mindlessDancer =
  enemyWith
    MindlessDancer
    Cards.mindlessDancer
    (6, Static 5, 3)
    (2, 1)
    ( spawnAtL
        ?~ SpawnAt (IncludeEmptySpace $ FarthestLocationFromYou $ locationIs Locations.emptySpace)
    )

instance HasAbilities MindlessDancer where
  getAbilities (MindlessDancer a) =
    withBaseAbilities a
      $ [ limitedAbility (GroupLimit PerRound 1)
            $ mkAbility a 1
            $ ForcedAbility
            $ EnemyMovedTo Timing.After (IncludeEmptySpace $ locationIs Locations.emptySpace) MovedViaHunter
            $ EnemyWithId (toId a)
        ]

instance HasModifiersFor MindlessDancer where
  getModifiersFor target (MindlessDancer attrs) | isTarget attrs target = pure $ toModifiers attrs [CanEnterEmptySpace]
  getModifiersFor _ _ = pure []

instance RunMessage MindlessDancer where
  runMessage msg e@(MindlessDancer attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      lead <- getLead
      pushAll
        $ [ CheckWindow
              [lead]
              [ Window.mkWindow
                  Timing.When
                  (Window.MovedFromHunter $ toId attrs)
              ]
          , HunterMove (toId attrs)
          ]
      pure e
    _ -> MindlessDancer <$> runMessage msg attrs
