module Arkham.Agenda.Cards.SwallowedSky (
  SwallowedSky (..),
  swallowedSky,
) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Resolution
import Arkham.Trait

newtype SwallowedSky = SwallowedSky AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

swallowedSky :: AgendaCard SwallowedSky
swallowedSky = agenda (3, C) SwallowedSky Cards.swallowedSky (Static 8)

instance HasModifiersFor SwallowedSky where
  getModifiersFor (SwallowedSky a) = do
    modifySelect a (EnemyWithTrait Monster) [EnemyFight 1]

instance RunMessage SwallowedSky where
  runMessage msg a@(SwallowedSky attrs) = case msg of
    AdvanceAgenda aid
      | aid == toId attrs && onSide D attrs ->
          a <$ push (ScenarioResolution $ Resolution 3)
    _ -> SwallowedSky <$> runMessage msg attrs
