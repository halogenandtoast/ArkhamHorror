module Arkham.Agenda.Cards.TheRitualBeginsBlackStarsRise (
  TheRitualBeginsBlackStarsRise (..),
  theRitualBeginsBlackStarsRise,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Helpers
import Arkham.Agenda.Runner
import Arkham.Agenda.Sequence qualified as AS
import Arkham.Campaigns.ThePathToCarcosa.Helpers
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Projection

newtype TheRitualBeginsBlackStarsRise = TheRitualBeginsBlackStarsRise AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

theRitualBeginsBlackStarsRise :: AgendaCard TheRitualBeginsBlackStarsRise
theRitualBeginsBlackStarsRise =
  agenda
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
        <> [advanceAgendaDeck attrs]
      pure a
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      investigatorIds <- getInvestigatorIds
      drawing <- for investigatorIds $ \iid -> drawCards iid (toAbilitySource attrs 1) 1
      pushAll
        $ PlaceDoom (toAbilitySource attrs 1) (toTarget attrs) 1
        : AdvanceAgendaIfThresholdSatisfied
        : drawing
      pure a
    _ -> TheRitualBeginsBlackStarsRise <$> runMessage msg attrs
