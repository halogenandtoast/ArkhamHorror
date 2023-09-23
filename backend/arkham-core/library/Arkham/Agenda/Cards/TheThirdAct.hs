module Arkham.Agenda.Cards.TheThirdAct (
  TheThirdAct (..),
  theThirdAct,
) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Matcher
import Arkham.Message

newtype TheThirdAct = TheThirdAct AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theThirdAct :: AgendaCard TheThirdAct
theThirdAct = agenda (1, A) TheThirdAct Cards.theThirdAct (Static 6)

instance RunMessage TheThirdAct where
  runMessage msg a@(TheThirdAct attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && onSide B attrs -> do
      royalEmissary <-
        selectJust
          $ ExtendedCardWithOneOf
            [ SetAsideCardMatch $ cardIs Cards.royalEmissary
            , VictoryDisplayCardMatch $ cardIs Cards.royalEmissary
            ]

      createRoyalEmissary <-
        createEnemyAtLocationMatching_ royalEmissary
          $ locationIs Cards.theatre

      pushAll
        [createRoyalEmissary, AdvanceAgendaDeck agendaDeckId (toSource attrs)]
      pure a
    _ -> TheThirdAct <$> runMessage msg attrs
