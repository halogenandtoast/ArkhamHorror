module Arkham.Enemy.Cards.SeekerOfCarcosa (
  seekerOfCarcosa,
  SeekerOfCarcosa (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message
import Arkham.Phase
import Arkham.Projection
import Arkham.Timing qualified as Timing

newtype SeekerOfCarcosa = SeekerOfCarcosa EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

seekerOfCarcosa :: EnemyCard SeekerOfCarcosa
seekerOfCarcosa =
  enemyWith
    SeekerOfCarcosa
    Cards.seekerOfCarcosa
    (2, Static 3, 2)
    (0, 1)
    ( spawnAtL
        ?~ SpawnLocation (EmptyLocation <> LocationWithTitle "Historical Society")
    )

instance HasAbilities SeekerOfCarcosa where
  getAbilities (SeekerOfCarcosa attrs) =
    withBaseAbilities
      attrs
      [ mkAbility attrs 1 $
          ForcedAbility $
            PhaseEnds Timing.When $
              PhaseIs
                MythosPhase
      ]

instance RunMessage SeekerOfCarcosa where
  runMessage msg e@(SeekerOfCarcosa attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      enemyLocation <- field EnemyLocation (toId attrs)
      for_ enemyLocation $ \loc -> do
        clueCount <- field LocationClues loc
        pushAll $
          if clueCount > 0
            then
              [ RemoveClues (toAbilitySource attrs 1) (toTarget loc) 1
              , PlaceClues (toAbilitySource attrs 1) (toTarget attrs) 1
              ]
            else [PlaceDoom (toAbilitySource attrs 1) (toTarget attrs) 1]
      pure e
    _ -> SeekerOfCarcosa <$> runMessage msg attrs
