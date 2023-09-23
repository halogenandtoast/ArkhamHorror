module Arkham.Agenda.Cards.CrossroadsOfFate (
  CrossroadsOfFate (..),
  crossroadsOfFate,
) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype CrossroadsOfFate = CrossroadsOfFate AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crossroadsOfFate :: AgendaCard CrossroadsOfFate
crossroadsOfFate = agenda (2, A) CrossroadsOfFate Cards.crossroadsOfFate (Static 10)

instance RunMessage CrossroadsOfFate where
  runMessage msg a@(CrossroadsOfFate attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
        investigators <- getInvestigators

        pushAll
          $ map (InvestigatorDefeated (toSource attrs)) investigators
          <> [SufferTrauma investigator 1 0 | investigator <- investigators]
          <> [R5]
        pure a
      _ -> CrossroadsOfFate <$> runMessage msg attrs
