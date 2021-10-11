module Arkham.Types.Agenda.Cards.LockedInside
  ( LockedInside
  , lockedInside
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Helpers
import Arkham.Types.Agenda.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Scenario.Deck
import Arkham.Types.Target
import Arkham.Types.Trait

newtype LockedInside = LockedInside AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lockedInside :: AgendaCard LockedInside
lockedInside = agenda (1, A) LockedInside Cards.lockedInside (Static 2)

instance AgendaRunner env => RunMessage env LockedInside where
  runMessage msg a@(LockedInside attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      lunatics <- mapMaybe (preview _EncounterCard) <$> getSetAsideCardsMatching
        (CardWithTrait Lunatic <> CardWithType EnemyType)
      a <$ pushAll
        [ ShuffleIntoEncounterDeck lunatics
        , ShuffleEncounterDiscardBackIn
        , DrawRandomFromScenarioDeck
          leadInvestigatorId
          MonstersDeck
          (toTarget attrs)
          1
        , AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)
        ]
    DrewFromScenarioDeck _ _ target cards | isTarget attrs target ->
      a <$ push (PlaceUnderneath ActDeckTarget cards)
    _ -> LockedInside <$> runMessage msg attrs
