module Arkham.Agenda.Cards.Intruders (
  Intruders (..),
  intruders,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Helpers.Location
import Arkham.Matcher hiding (InvestigatorDefeated)
import Arkham.Message
import Arkham.Scenario.Deck
import Arkham.Treachery.Cards qualified as Treacheries

newtype Intruders = Intruders AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

intruders :: AgendaCard Intruders
intruders = agenda (2, A) Intruders Cards.intruders (Static 9)

instance HasAbilities Intruders where
  getAbilities (Intruders a) =
    [ restrictedAbility a 1 (ScenarioDeckWithCard ExplorationDeck)
        $ ActionAbility (Just Action.Explore)
        $ ActionCost 1
    ]

instance RunMessage Intruders where
  runMessage msg a@(Intruders attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      locationSymbols <- toConnections =<< getJustLocation iid
      push
        $ Explore
          iid
          source
          (CardWithOneOf $ map CardWithPrintedLocationSymbol locationSymbols)
      pure a
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      iids <- getInvestigatorIds
      unpoisoned <-
        selectList
          $ NotInvestigator
          $ HasMatchingTreachery
          $ treacheryIs
          $ Treacheries.poisoned
      pushAll
        $ [InvestigatorDefeated (toSource attrs) iid | iid <- iids]
        <> [ AddCampaignCardToDeck iid Treacheries.poisoned
           | iid <- unpoisoned
           ]
      pure a
    _ -> Intruders <$> runMessage msg attrs
