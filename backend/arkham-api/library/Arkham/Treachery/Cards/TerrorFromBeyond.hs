module Arkham.Treachery.Cards.TerrorFromBeyond (terrorFromBeyond) where

import Arkham.Card
import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype TerrorFromBeyond = TerrorFromBeyond TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

terrorFromBeyond :: TreacheryCard TerrorFromBeyond
terrorFromBeyond = treachery TerrorFromBeyond Cards.terrorFromBeyond

instance RunMessage TerrorFromBeyond where
  runMessage msg t@(TerrorFromBeyond attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> withI18n do
      iids <- getInvestigators
      withHand <- forToSnd iids $ field InvestigatorHand
      firstCopy <- isFirstCopyThisPhase attrs
      chooseNM iid (if firstCopy then 1 else 2) do
        for_ [("assets", AssetCard), ("events", EventCard), ("skills", SkillCard)] \(label, cardType) -> do
          labeled' label do
            for_ withHand \(iid', hand) -> do
              for_ (filterCards cardType hand) $ discardCard iid' attrs
      pure t
    _ -> TerrorFromBeyond <$> liftRunMessage msg attrs
