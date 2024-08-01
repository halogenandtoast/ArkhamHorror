module Arkham.Event.Cards.ThoroughInquiry (thoroughInquiry, ThoroughInquiry (..)) where

import Arkham.Capability
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Name

newtype ThoroughInquiry = ThoroughInquiry EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thoroughInquiry :: EventCard ThoroughInquiry
thoroughInquiry = event ThoroughInquiry Cards.thoroughInquiry

instance RunMessage ThoroughInquiry where
  runMessage msg e@(ThoroughInquiry attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      investigators <-
        selectWithField InvestigatorName $ affectsOthers $ colocatedWith iid <> can.draw.cards
      case investigators of
        [] -> pure ()
        [(iid', _)] -> drawCardsIfCan iid' attrs 5
        _ ->
          chooseAmounts
            iid
            "Cards each player should draw"
            (TotalAmountTarget 5)
            (map (\(_, name) -> (toTitle name, (0, 5))) investigators)
            attrs
      pure e
    ResolveAmounts _ amounts (isTarget attrs -> True) -> do
      for_ amounts $ \(name, n) -> do
        whenJustM (selectOne $ InvestigatorWithTitle name) \iid ->
          drawCardsIfCan iid attrs n
      pure e
    _ -> ThoroughInquiry <$> liftRunMessage msg attrs
