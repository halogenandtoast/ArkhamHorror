module Arkham.Agenda.Cards.TheRitualBeginsBlackStarsRise
  ( TheRitualBeginsBlackStarsRise(..)
  , theRitualBeginsBlackStarsRise
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Helpers
import Arkham.Agenda.Runner
import Arkham.Agenda.Sequence qualified as AS
import Arkham.Campaigns.ThePathToCarcosa.Helpers
import Arkham.Classes
import Arkham.Cost
import Arkham.Deck qualified as Deck
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection

newtype TheRitualBeginsBlackStarsRise = TheRitualBeginsBlackStarsRise AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theRitualBeginsBlackStarsRise :: AgendaCard TheRitualBeginsBlackStarsRise
theRitualBeginsBlackStarsRise = agenda
  (1, C)
  TheRitualBeginsBlackStarsRise
  Cards.theRitualBeginsBlackStarsRise
  (Static 5)

instance HasAbilities TheRitualBeginsBlackStarsRise where
  getAbilities (TheRitualBeginsBlackStarsRise a) =
    [ limitedAbility (GroupLimit PerRound 1)
        $ mkAbility a 1
        $ FastAbility
        $ GroupClueCost (PerPlayer 1) Anywhere
    ]

instance RunMessage TheRitualBeginsBlackStarsRise where
  runMessage msg a@(TheRitualBeginsBlackStarsRise attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide D attrs -> do
      riftSeekers <- getSetAsideCardsMatching (CardWithTitle "Rift Seeker")
      mAgenda1A <- selectOne $ AgendaWithSequence $ AS.Sequence 1 A
      markDoubtOrConviction <- case mAgenda1A of
        Nothing -> pure []
        Just a1aId -> do
          a1aDoom <- field AgendaDoom a1aId
          markMsg <- if a1aDoom > 3 then markDoubt else markConviction
          pure [markMsg]
      pushAll
        $ [ ShuffleEncounterDiscardBackIn
          , ShuffleCardsIntoDeck Deck.EncounterDeck riftSeekers
          ]
        <> markDoubtOrConviction
        <> [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
      pure a
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      investigatorIds <- getInvestigatorIds
      pushAll
        $ [PlaceDoom (toTarget attrs) 1, AdvanceAgendaIfThresholdSatisfied]
        <> [ drawCards iid attrs 1 | iid <- investigatorIds ]
      pure a
    _ -> TheRitualBeginsBlackStarsRise <$> runMessage msg attrs
