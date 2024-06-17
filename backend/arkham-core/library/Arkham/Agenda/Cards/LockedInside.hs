module Arkham.Agenda.Cards.LockedInside (
  LockedInside (..),
  lockedInside,
) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Helpers
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.Draw.Types
import Arkham.GameValue
import Arkham.Scenario.Deck

newtype LockedInside = LockedInside AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lockedInside :: AgendaCard LockedInside
lockedInside = agenda (1, A) LockedInside Cards.lockedInside (Static 2)

instance RunMessage LockedInside where
  runMessage msg a@(LockedInside attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      lead <- getLeadInvestigatorId
      pushAll
        [ ShuffleScenarioDeckIntoEncounterDeck LunaticsDeck
        , ShuffleEncounterDiscardBackIn
        , DrawCards lead $ randomTargetCardDraw attrs MonstersDeck 1
        , AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)
        ]
      pure a
    DrewCards _ drew | maybe False (isTarget attrs) drew.target -> do
      push $ PlaceUnderneath ActDeckTarget drew.cards
      pure a
    _ -> LockedInside <$> runMessage msg attrs
