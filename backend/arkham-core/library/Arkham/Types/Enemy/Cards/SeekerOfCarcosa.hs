module Arkham.Types.Enemy.Cards.SeekerOfCarcosa
  ( seekerOfCarcosa
  , SeekerOfCarcosa(..)
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Phase
import Arkham.Types.Query
import Arkham.Types.Target
import Arkham.Types.Timing qualified as Timing

newtype SeekerOfCarcosa = SeekerOfCarcosa EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

seekerOfCarcosa :: EnemyCard SeekerOfCarcosa
seekerOfCarcosa = enemyWith
  SeekerOfCarcosa
  Cards.seekerOfCarcosa
  (2, Static 3, 2)
  (0, 1)
  (spawnAtL ?~ EmptyLocation <> LocationWithTitle "Historical Society")

instance HasAbilities SeekerOfCarcosa where
  getAbilities (SeekerOfCarcosa attrs) = withBaseAbilities
    attrs
    [ mkAbility attrs 1 $ ForcedAbility $ PhaseEnds Timing.When $ PhaseIs
        MythosPhase
    ]

instance EnemyRunner env => RunMessage env SeekerOfCarcosa where
  runMessage msg e@(SeekerOfCarcosa attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      clueCount <- unClueCount <$> getCount (enemyLocation attrs)
      e <$ pushAll
        (if clueCount > 0
          then
            [ RemoveClues (LocationTarget $ enemyLocation attrs) 1
            , PlaceClues (toTarget attrs) 1
            ]
          else [PlaceDoom (toTarget attrs) 1]
        )
    _ -> SeekerOfCarcosa <$> runMessage msg attrs
