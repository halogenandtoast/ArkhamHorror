-- Scaffold: printed card abilities are not implemented yet.
module Arkham.Homebrew.NightOfVespers.Agendas.TooSwiftArrives (tooSwiftArrives) where

import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Query (getLead)
import Arkham.Homebrew.NightOfVespers.CardDefs.Agendas qualified as Cards
import Arkham.Matcher.Investigator
import Arkham.Matcher.Location

newtype TooSwiftArrives = TooSwiftArrives AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tooSwiftArrives :: AgendaCard TooSwiftArrives
tooSwiftArrives = agenda (1, A) TooSwiftArrives Cards.tooSwiftArrives (Static 1)

instance HasModifiersFor TooSwiftArrives where
  getModifiersFor (TooSwiftArrives a) = do
    modifySelect a (InvestigatorAt "Trattoria del Parche") [CannotMove, CannotBeMoved]

instance RunMessage TooSwiftArrives where
  runMessage msg a@(TooSwiftArrives attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      lead <- getLead
      withMatch (location_ "Trattoria del Parche") $ flipOver lead
      advanceTheAct attrs
      advanceAgendaDeck attrs
      pure a
    _ -> TooSwiftArrives <$> liftRunMessage msg attrs
