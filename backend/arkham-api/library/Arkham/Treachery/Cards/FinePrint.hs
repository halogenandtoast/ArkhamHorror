module Arkham.Treachery.Cards.FinePrint (finePrint) where

import Arkham.Card
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Treachery.Import.Lifted

newtype FinePrint = FinePrint TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

finePrint :: TreacheryCard FinePrint
finePrint = treachery FinePrint Cards.finePrint

instance RunMessage FinePrint where
  runMessage msg t@(FinePrint attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      hasResources <- fieldMap InvestigatorResources (>= 7) iid
      if hasResources
        then loseResources iid attrs 7
        else do
          case toCard attrs of
            EncounterCard _ -> error "should be player card"
            VengeanceCard _ -> error "should be player card"
            PlayerCard pc -> do
              removeCardFromDeckForCampaign iid pc
              addCampaignCardToDeck iid =<< genPlayerCard Treacheries.sellYourSoul
      pure t
    _ -> FinePrint <$> liftRunMessage msg attrs
