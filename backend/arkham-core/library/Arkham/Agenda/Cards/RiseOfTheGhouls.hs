module Arkham.Agenda.Cards.RiseOfTheGhouls (
  RiseOfTheGhouls (..),
  riseOfTheGhouls,
) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Helpers
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Trait

newtype RiseOfTheGhouls = RiseOfTheGhouls AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

riseOfTheGhouls :: AgendaCard RiseOfTheGhouls
riseOfTheGhouls = agenda (2, A) RiseOfTheGhouls Cards.riseOfTheGhouls (Static 7)

instance RunMessage RiseOfTheGhouls where
  runMessage msg a@(RiseOfTheGhouls attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      lead <- getLead
      pushAll
        [ ShuffleEncounterDiscardBackIn
        , DiscardUntilFirst lead (AgendaSource aid) Deck.EncounterDeck
            $ basic (#enemy <> withTrait Ghoul)
        ]
      pure a
    RequestedEncounterCard (AgendaSource aid) _ mcard | aid == toId attrs -> do
      case mcard of
        Nothing -> push $ advanceAgendaDeck attrs
        Just card -> do
          lead <- getLead
          pushAll
            [ InvestigatorDrewEncounterCard lead card
            , advanceAgendaDeck attrs
            ]
      pure a
    _ -> RiseOfTheGhouls <$> runMessage msg attrs
