module Arkham.Agenda.Cards.TheCityFloods
  ( TheCityFloods(..)
  , theCityFloods
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Message
import Arkham.Resolution
import Arkham.Target
import Arkham.Treachery.Cards qualified as Treacheries

newtype TheCityFloods = TheCityFloods AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCityFloods :: AgendaCard TheCityFloods
theCityFloods = agenda (3, A) TheCityFloods Cards.theCityFloods (Static 8)

instance HasModifiersFor TheCityFloods where
  getModifiersFor (TreacheryTarget tid) (TheCityFloods a) = do
    isAncientEvils <- member tid
      <$> select (treacheryIs Treacheries.ancientEvils)
    pure $ toModifiers a [ AddKeyword Keyword.Surge | isAncientEvils ]
  getModifiersFor _ _ = pure []

instance RunMessage TheCityFloods where
  runMessage msg a@(TheCityFloods attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
      a <$ push (ScenarioResolution $ Resolution 3)
    _ -> TheCityFloods <$> runMessage msg attrs
