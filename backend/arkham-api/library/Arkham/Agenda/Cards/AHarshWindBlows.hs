module Arkham.Agenda.Cards.AHarshWindBlows (AHarshWindBlows (..), aHarshWindBlows) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Query (getLead)
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype AHarshWindBlows = AHarshWindBlows AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aHarshWindBlows :: AgendaCard AHarshWindBlows
aHarshWindBlows = agenda (4, A) AHarshWindBlows Cards.aHarshWindBlows (Static 5)

instance RunMessage AHarshWindBlows where
  runMessage msg a@(AHarshWindBlows attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      lead <- getLead
      chooseSelectM lead (LocationWithCardsUnderneath AnyCards) (handleTarget lead attrs)
      advanceAgendaDeck attrs
      pure a
    HandleTargetChoice _ (isSource attrs -> True) (LocationTarget location) -> do
      lead <- getLead
      cards <- field LocationCardsUnderneath location
      focusCards cards (continue lead . pure)
      pure a
    _ -> AHarshWindBlows <$> liftRunMessage msg attrs
