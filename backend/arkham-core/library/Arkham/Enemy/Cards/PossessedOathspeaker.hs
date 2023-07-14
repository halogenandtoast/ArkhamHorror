module Arkham.Enemy.Cards.PossessedOathspeaker (
  possessedOathspeaker,
  PossessedOathspeaker (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Helpers.Agenda
import Arkham.Matcher
import Arkham.Message hiding (EnemyDefeated)
import Arkham.Phase
import Arkham.Resolution
import Arkham.Timing qualified as Timing
import Arkham.Token

newtype PossessedOathspeaker = PossessedOathspeaker EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

possessedOathspeaker :: EnemyCard PossessedOathspeaker
possessedOathspeaker =
  enemy
    PossessedOathspeaker
    Cards.possessedOathspeaker
    (4, PerPlayer 5, 3)
    (2, 2)

instance HasModifiersFor PossessedOathspeaker where
  getModifiersFor (EnemyTarget eid) (PossessedOathspeaker attrs)
    | toId attrs == eid = do
        step <- getCurrentAgendaStep
        pure $ toModifiers attrs [CannotBeDamaged | step == 1 || step == 2]
  getModifiersFor _ _ = pure []

instance HasAbilities PossessedOathspeaker where
  getAbilities (PossessedOathspeaker a) =
    withBaseAbilities
      a
      [ mkAbility a 1 $
          ForcedAbility $
            PhaseBegins Timing.When $
              PhaseIs
                EnemyPhase
      , mkAbility a 2 $
          ForcedAbility $
            EnemyDefeated Timing.When Anyone ByAny $
              EnemyWithId $
                toId a
      ]

instance RunMessage PossessedOathspeaker where
  runMessage msg e@(PossessedOathspeaker attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      leadInvestigatorIdL <- getLeadInvestigatorId
      push $
        if enemyExhausted attrs
          then
            chooseOne
              leadInvestigatorIdL
              [ Label "Ready Possessed Oathspeaker" [Ready (toTarget attrs)]
              , Label
                  "Place 1 doom on Possessed Oathspeaker"
                  [PlaceTokens (toSource attrs) (toTarget attrs) Doom 1]
              ]
          else PlaceTokens (toSource attrs) (toTarget attrs) Doom 1
      pure e
    UseCardAbility _ source 2 _ _ | isSource attrs source -> do
      e <$ push (ScenarioResolution $ Resolution 3)
    _ -> PossessedOathspeaker <$> runMessage msg attrs
