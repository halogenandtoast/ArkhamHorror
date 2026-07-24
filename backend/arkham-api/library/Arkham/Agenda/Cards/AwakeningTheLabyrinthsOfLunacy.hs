module Arkham.Agenda.Cards.AwakeningTheLabyrinthsOfLunacy (awakeningTheLabyrinthsOfLunacy) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher

newtype AwakeningTheLabyrinthsOfLunacy = AwakeningTheLabyrinthsOfLunacy AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

awakeningTheLabyrinthsOfLunacy :: AgendaCard AwakeningTheLabyrinthsOfLunacy
awakeningTheLabyrinthsOfLunacy =
  agenda (1, A) AwakeningTheLabyrinthsOfLunacy Cards.awakeningTheLabyrinthsOfLunacy (Static 6)

instance RunMessage AwakeningTheLabyrinthsOfLunacy where
  runMessage msg a@(AwakeningTheLabyrinthsOfLunacy attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      lead <- getLead
      actId <- selectJust AnyAct
      push $ AdvanceAct actId (toSource lead) #other
      advanceAgendaDeck attrs
      pure a
    _ -> AwakeningTheLabyrinthsOfLunacy <$> liftRunMessage msg attrs
