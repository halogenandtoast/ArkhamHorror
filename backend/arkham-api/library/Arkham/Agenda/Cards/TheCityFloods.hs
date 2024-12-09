module Arkham.Agenda.Cards.TheCityFloods (TheCityFloods (..), theCityFloods) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Card
import Arkham.Classes
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.GameValue
import Arkham.Keyword qualified as Keyword
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Treacheries

newtype TheCityFloods = TheCityFloods AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCityFloods :: AgendaCard TheCityFloods
theCityFloods = agenda (3, A) TheCityFloods Cards.theCityFloods (Static 8)

instance HasModifiersFor TheCityFloods where
  getModifiersFor (TheCityFloods a) = do
    ancientEvils <- findAllCards (`isCard` Treacheries.ancientEvils)
    modifyEach a ancientEvils [AddKeyword Keyword.Surge]

instance RunMessage TheCityFloods where
  runMessage msg a@(TheCityFloods attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      push R3
      pure a
    _ -> TheCityFloods <$> runMessage msg attrs
