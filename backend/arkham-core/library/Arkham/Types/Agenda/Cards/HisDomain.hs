module Arkham.Types.Agenda.Cards.HisDomain
  ( HisDomain
  , hisDomain
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Matcher hiding (InvestigatorDefeated)
import Arkham.Types.Message

newtype HisDomain = HisDomain AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hisDomain :: AgendaCard HisDomain
hisDomain = agenda (3, A) HisDomain Cards.hisDomain (Static 8)

instance AgendaRunner env => RunMessage env HisDomain where
  runMessage msg a@(HisDomain attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      investigatorIds <- selectList $ UneliminatedInvestigator
      a <$ pushAll
        [ InvestigatorDefeated (toSource attrs) iid | iid <- investigatorIds ]
    _ -> HisDomain <$> runMessage msg attrs
