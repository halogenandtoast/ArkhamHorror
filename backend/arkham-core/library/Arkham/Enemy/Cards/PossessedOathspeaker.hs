module Arkham.Enemy.Cards.PossessedOathspeaker
  ( possessedOathspeaker
  , PossessedOathspeaker(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Classes
import Arkham.Enemy.Runner
import Arkham.Id
import Arkham.Matcher
import Arkham.Message hiding (EnemyDefeated)
import Arkham.Modifier
import Arkham.Phase
import Arkham.Resolution
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype PossessedOathspeaker = PossessedOathspeaker EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

possessedOathspeaker :: EnemyCard PossessedOathspeaker
possessedOathspeaker = enemy
  PossessedOathspeaker
  Cards.possessedOathspeaker
  (4, PerPlayer 5, 3)
  (2, 2)

instance HasStep AgendaStep env () => HasModifiersFor env PossessedOathspeaker where
  getModifiersFor _ (EnemyTarget eid) (PossessedOathspeaker attrs)
    | toId attrs == eid = do
      step <- unAgendaStep <$> getStep ()
      pure $ toModifiers attrs [ CannotBeDamaged | step == 1 || step == 2 ]
  getModifiersFor _ _ _ = pure []

instance HasAbilities PossessedOathspeaker where
  getAbilities (PossessedOathspeaker a) = withBaseAbilities
    a
    [ mkAbility a 1 $ ForcedAbility $ PhaseBegins Timing.When $ PhaseIs
      EnemyPhase
    , mkAbility a 2
    $ ForcedAbility
    $ EnemyDefeated Timing.When Anyone
    $ EnemyWithId
    $ toId a
    ]

instance EnemyRunner env => RunMessage env PossessedOathspeaker where
  runMessage msg e@(PossessedOathspeaker attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      leadInvestigatorIdL <- getLeadInvestigatorId
      e <$ push
        (if enemyExhausted attrs
          then chooseOne
            leadInvestigatorIdL
            [ Label "Ready Possessed Oathspeaker" [Ready (toTarget attrs)]
            , Label
              "Place 1 doom on Possessed Oathspeaker"
              [PlaceDoom (toTarget attrs) 1]
            ]
          else PlaceDoom (toTarget attrs) 1
        )
    UseCardAbility _ source _ 2 _ | isSource attrs source ->
      e <$ push (ScenarioResolution $ Resolution 3)
    _ -> PossessedOathspeaker <$> runMessage msg attrs
