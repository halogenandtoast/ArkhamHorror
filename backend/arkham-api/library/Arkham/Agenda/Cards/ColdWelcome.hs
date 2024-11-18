module Arkham.Agenda.Cards.ColdWelcome (ColdWelcome (..), coldWelcome) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Card.CardDef
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Helpers.Window (entering)
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype ColdWelcome = ColdWelcome AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coldWelcome :: AgendaCard ColdWelcome
coldWelcome = agenda (1, A) ColdWelcome Cards.coldWelcome (Static 4)

instance HasAbilities ColdWelcome where
  getAbilities (ColdWelcome a) = [mkAbility a 1 $ forced $ Enters #after You $ ConnectedToSetAsideLocation]

instance RunMessage ColdWelcome where
  runMessage msg a@(ColdWelcome attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    UseCardAbility _iid (isSource attrs -> True) 1 (entering -> lid) _ -> do
      symbol <- field LocationPrintedSymbol lid
      let isConnected = elem symbol . cdLocationConnections . toCardDef
      setAsideLocations <- filter isConnected <$> getSetAsideCardsMatching #location
      traverse_ placeLocation_ setAsideLocations
      pure a
    _ -> ColdWelcome <$> liftRunMessage msg attrs
