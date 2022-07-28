module Arkham.Agenda.Cards.SwallowedSky
  ( SwallowedSky(..)
  , swallowedSky
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.Enemy.Types ( Field (EnemyTraits) )
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Message
import Arkham.Projection
import Arkham.Resolution
import Arkham.Target
import Arkham.Trait

newtype SwallowedSky = SwallowedSky AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

swallowedSky :: AgendaCard SwallowedSky
swallowedSky = agenda (3, C) SwallowedSky Cards.swallowedSky (Static 8)

instance HasModifiersFor SwallowedSky where
  getModifiersFor _ (EnemyTarget eid) (SwallowedSky a) = do
    isMonster <- fieldP EnemyTraits (member Monster) eid
    pure $ toModifiers a [ EnemyFight 1 | isMonster ]
  getModifiersFor _ _ _ = pure []

instance RunMessage SwallowedSky where
  runMessage msg a@(SwallowedSky attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide D attrs ->
      a <$ push (ScenarioResolution $ Resolution 3)
    _ -> SwallowedSky <$> runMessage msg attrs
