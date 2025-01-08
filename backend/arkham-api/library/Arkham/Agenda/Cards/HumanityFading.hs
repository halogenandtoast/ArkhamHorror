module Arkham.Agenda.Cards.HumanityFading (
  HumanityFading (..),
  humanityFading,
) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher hiding (InvestigatorDefeated)

newtype HumanityFading = HumanityFading AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

humanityFading :: AgendaCard HumanityFading
humanityFading = agenda (3, A) HumanityFading Cards.humanityFading (Static 7)

instance HasModifiersFor HumanityFading where
  getModifiersFor (HumanityFading attrs) =
    if onSide A attrs
      then modifySelect attrs Anyone [HandSize (-4)]
      else pure mempty

instance RunMessage HumanityFading where
  runMessage msg a@(HumanityFading attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      iids <- select UneliminatedInvestigator
      pushAll $ map (InvestigatorDefeated (toSource attrs)) iids
      pure a
    _ -> HumanityFading <$> runMessage msg attrs
