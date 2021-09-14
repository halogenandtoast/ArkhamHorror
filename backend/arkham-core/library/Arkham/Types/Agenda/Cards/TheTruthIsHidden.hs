module Arkham.Types.Agenda.Cards.TheTruthIsHidden
  ( TheTruthIsHidden
  , theTruthIsHidden
  ) where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Message

newtype TheTruthIsHidden = TheTruthIsHidden AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTruthIsHidden :: AgendaCard TheTruthIsHidden
theTruthIsHidden =
  agenda (1, A) TheTruthIsHidden Cards.theTruthIsHidden (Static 12)

instance AgendaRunner env => RunMessage env TheTruthIsHidden where
  runMessage msg a@(TheTruthIsHidden attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
      a <$ pushAll [NextAgenda aid "TODO"]
    _ -> TheTruthIsHidden <$> runMessage msg attrs
