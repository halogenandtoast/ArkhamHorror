module Arkham.Agenda.Cards.SettingSun (
  SettingSun (..),
  settingSun,
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
import Arkham.Matcher
import Arkham.Scenario.Deck

newtype SettingSun = SettingSun AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

settingSun :: AgendaCard SettingSun
settingSun = agenda (2, A) SettingSun Cards.settingSun (Static 5)

instance HasAbilities SettingSun where
  getAbilities (SettingSun a) =
    [ restrictedAbility a 1 (ScenarioDeckWithCard ExplorationDeck)
        $ ActionAbility [Action.Explore]
        $ ActionCost 1
    ]

instance RunMessage SettingSun where
  runMessage msg a@(SettingSun attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      locationSymbols <- toConnections =<< getJustLocation iid
      push
        $ Explore
          iid
          source
          (CardWithOneOf $ map CardWithPrintedLocationSymbol locationSymbols)
      pure a
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      iids <- selectList UneliminatedInvestigator
      pushAll $ map Resign iids
      pure a
    _ -> SettingSun <$> runMessage msg attrs
