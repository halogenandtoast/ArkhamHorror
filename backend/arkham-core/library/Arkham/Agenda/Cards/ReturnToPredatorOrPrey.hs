module Arkham.Agenda.Cards.ReturnToPredatorOrPrey
  ( ReturnToPredatorOrPrey(..)
  , returnToPredatorOrPrey
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Action qualified as Action
import Arkham.Agenda.Types
import Arkham.Agenda.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Cost
import Arkham.GameValue
import Arkham.Message

newtype ReturnToPredatorOrPrey = ReturnToPredatorOrPrey AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToPredatorOrPrey :: AgendaCard ReturnToPredatorOrPrey
returnToPredatorOrPrey =
  agenda (1, A) ReturnToPredatorOrPrey Cards.returnToPredatorOrPrey (Static 6)

instance HasAbilities ReturnToPredatorOrPrey where
  getAbilities (ReturnToPredatorOrPrey attrs) =
    [mkAbility attrs 1 $ ActionAbility (Just Action.Resign) (ActionCost 1)]

instance RunMessage ReturnToPredatorOrPrey where
  runMessage msg a@(ReturnToPredatorOrPrey attrs@AgendaAttrs {..}) =
    case msg of
      AdvanceAgenda aid | aid == agendaId && onSide B attrs -> do
        narogath <- EncounterCard <$> genEncounterCard Enemies.narogath
        createNarogath <- createEnemyEngagedWithPrey_ narogath
        pushAll
          [ createNarogath
          , AdvanceAgendaDeck agendaDeckId (toSource attrs)
          ]
        pure a
      UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
        push $ Resign iid
        pure a
      _ -> ReturnToPredatorOrPrey <$> runMessage msg attrs
