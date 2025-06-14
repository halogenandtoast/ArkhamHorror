module Arkham.Treachery.Cards.WindowToAnotherTime (windowToAnotherTime) where

import Arkham.I18n
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (LocationCard)
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Deck
import Arkham.Trait (Trait (Ancient))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype WindowToAnotherTime = WindowToAnotherTime TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

windowToAnotherTime :: TreacheryCard WindowToAnotherTime
windowToAnotherTime = treachery WindowToAnotherTime Cards.windowToAnotherTime

instance RunMessage WindowToAnotherTime where
  runMessage msg t@(WindowToAnotherTime attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      ancientLocations <- selectWithField LocationCard $ LocationWithTrait Ancient
      chooseOrRunOneM iid $ withI18n do
        labeled' "placeAgendaDoomCanAdvance" $ placeDoomOnAgendaAndCheckAdvance 1
        for_ ancientLocations \(lid, card) -> do
          targeting lid do
            removeLocation lid
            shuffleCardsIntoDeck ExplorationDeck [card]
      pure t
    _ -> WindowToAnotherTime <$> liftRunMessage msg attrs
