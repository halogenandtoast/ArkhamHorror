module Arkham.Enemy.Cards.DiscipleOfTheDevourer
  ( discipleOfTheDevourer
  , DiscipleOfTheDevourer(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Types (Field(..))
import Arkham.Agenda.Sequence (agendaStep, AgendaStep(..))
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Classes
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Timing qualified as Timing

newtype DiscipleOfTheDevourer = DiscipleOfTheDevourer EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

discipleOfTheDevourer :: EnemyCard DiscipleOfTheDevourer
discipleOfTheDevourer = enemyWith
  DiscipleOfTheDevourer
  Cards.discipleOfTheDevourer
  (3, Static 1, 1)
  (1, 0)
  (spawnAtL ?~ FarthestLocationFromYou EmptyLocation)

instance HasAbilities DiscipleOfTheDevourer where
  getAbilities (DiscipleOfTheDevourer x) = withBaseAbilities
    x
    [ mkAbility x 1
      $ ForcedAbility
      $ EnemySpawns Timing.After Anywhere
      $ EnemyWithId
      $ toId x
    ]

instance RunMessage DiscipleOfTheDevourer where
  runMessage msg e@(DiscipleOfTheDevourer attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      let
        messages =
          [PlaceDoom (toTarget attrs) 1, InvestigatorPlaceCluesOnLocation iid 1]
      agendaId <- selectJust AnyAgenda
      step <- fieldMap AgendaSequence agendaStep agendaId
      e <$ if step == AgendaStep 1 then push (chooseOne iid messages) else pushAll messages
    _ -> DiscipleOfTheDevourer <$> runMessage msg attrs
