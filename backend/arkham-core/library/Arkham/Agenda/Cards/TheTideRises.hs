module Arkham.Agenda.Cards.TheTideRises
  ( TheTideRises(..)
  , theTideRises
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

newtype TheTideRises = TheTideRises AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTideRises :: AgendaCard TheTideRises
theTideRises = agenda (1, A) TheTideRises Cards.theTideRises (Static 5)

instance HasAbilities TheTideRises where
  getAbilities (TheTideRises a) =
    [ limitedAbility (GroupLimit PerRound 1)
        $ mkAbility a 1
        $ FastAbility
        $ GroupClueCost (PerPlayer 1) Anywhere
    ]

instance RunMessage TheTideRises where
  runMessage msg a@(TheTideRises attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      tidalTerrors <- getSetAsideCardsMatching (CardWithTitle "Tidal Terror")
      mAgenda1C <- selectOne $ AgendaWithSequence $ AS.Sequence 1 C
      markDoubtOrConviction <- case mAgenda1C of
        Nothing -> pure []
        Just a1cId -> do
          a1cDoom <- field AgendaDoom a1cId
          markMsg <- if a1cDoom > 3 then markDoubt else markConviction
          pure [markMsg]
      pushAll
        $ [ ShuffleEncounterDiscardBackIn
          , ShuffleCardsIntoDeck Deck.EncounterDeck tidalTerrors
          ]
        <> markDoubtOrConviction
        <> [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
      pure a
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      investigatorIds <- getInvestigatorIds
      pushAll
        $ [PlaceDoom (toTarget attrs) 1, AdvanceAgendaIfThresholdSatisfied]
        <> [ TakeResources iid 2 False | iid <- investigatorIds ]
      pure a
    _ -> TheTideRises <$> runMessage msg attrs
