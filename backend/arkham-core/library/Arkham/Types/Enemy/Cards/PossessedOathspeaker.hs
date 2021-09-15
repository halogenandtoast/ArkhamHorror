module Arkham.Types.Enemy.Cards.PossessedOathspeaker
  ( possessedOathspeaker
  , PossessedOathspeaker(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (EnemyDefeated)
import Arkham.Types.Modifier
import Arkham.Types.Phase
import Arkham.Types.Resolution
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing

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
  getAbilities (PossessedOathspeaker a) =
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
