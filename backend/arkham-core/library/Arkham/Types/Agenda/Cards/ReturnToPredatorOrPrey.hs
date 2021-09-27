module Arkham.Types.Agenda.Cards.ReturnToPredatorOrPrey
  ( ReturnToPredatorOrPrey(..)
  , returnToPredatorOrPrey
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Types.Ability
import Arkham.Types.Action qualified as Action
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Card
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Source

newtype ReturnToPredatorOrPrey = ReturnToPredatorOrPrey AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToPredatorOrPrey :: AgendaCard ReturnToPredatorOrPrey
returnToPredatorOrPrey =
  agenda (1, A) ReturnToPredatorOrPrey Cards.returnToPredatorOrPrey (Static 6)

instance HasAbilities ReturnToPredatorOrPrey where
  getAbilities (ReturnToPredatorOrPrey attrs) =
    [mkAbility attrs 1 $ ActionAbility (Just Action.Resign) (ActionCost 1)]

instance AgendaRunner env => RunMessage env ReturnToPredatorOrPrey where
  runMessage msg a@(ReturnToPredatorOrPrey attrs@AgendaAttrs {..}) =
    case msg of
      AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 1 B -> do
        narogath <- EncounterCard <$> genEncounterCard Enemies.narogath
        a <$ pushAll
          [CreateEnemyEngagedWithPrey narogath, NextAgenda aid "01122"]
      UseCardAbility iid (AgendaSource aid) _ 1 _ | aid == agendaId -> do
        push (Resign iid)
        ReturnToPredatorOrPrey <$> runMessage msg attrs
      _ -> ReturnToPredatorOrPrey <$> runMessage msg attrs
