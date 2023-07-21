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
import Arkham.Message

newtype HumanityFading = HumanityFading AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

humanityFading :: AgendaCard HumanityFading
humanityFading = agenda (3, A) HumanityFading Cards.humanityFading (Static 7)

instance HasModifiersFor HumanityFading where
  getModifiersFor (InvestigatorTarget _) (HumanityFading attrs)
    | onSide A attrs = pure $ toModifiers attrs [HandSize (-4)]
  getModifiersFor _ _ = pure []

instance RunMessage HumanityFading where
  runMessage msg a@(HumanityFading attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      iids <- selectList UneliminatedInvestigator
      pushAll $ map (InvestigatorDefeated (toSource attrs)) iids
      pure a
    _ -> HumanityFading <$> runMessage msg attrs
