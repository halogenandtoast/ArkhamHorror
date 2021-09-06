module Arkham.Types.Agenda.Cards.FashionablyLate
  ( FashionablyLate
  , fashionablyLate
  ) where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Message

newtype FashionablyLate = FashionablyLate AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fashionablyLate :: AgendaCard FashionablyLate
fashionablyLate =
  agenda (1, A) FashionablyLate Cards.fashionablyLate (Static 3)

instance AgendaRunner env => RunMessage env FashionablyLate where
  runMessage msg a@(FashionablyLate attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
      a <$ pushAll [NextAgenda aid "TODO"]
    _ -> FashionablyLate <$> runMessage msg attrs
