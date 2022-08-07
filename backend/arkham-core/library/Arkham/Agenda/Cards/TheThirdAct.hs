module Arkham.Agenda.Cards.TheThirdAct
  ( TheThirdAct(..)
  , theThirdAct
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Location.Cards qualified as Cards
import Arkham.Agenda.Types
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
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
      royalEmissary <- fromJustNote "missing royal emissary" <$> selectOne
        (ExtendedCardWithOneOf
          [ SetAsideCardMatch $ cardIs Cards.royalEmissary
          , VictoryDisplayCardMatch $ cardIs Cards.royalEmissary
          ]
        )

      a <$ pushAll
        [ CreateEnemyAtLocationMatching royalEmissary $ locationIs Cards.theatre
        , AdvanceAgendaDeck agendaDeckId (toSource attrs)
        ]
    _ -> TheThirdAct <$> runMessage msg attrs
