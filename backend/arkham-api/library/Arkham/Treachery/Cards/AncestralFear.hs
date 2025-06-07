module Arkham.Treachery.Cards.AncestralFear (ancestralFear) where

import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype AncestralFear = AncestralFear TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancestralFear :: TreacheryCard AncestralFear
ancestralFear = treachery AncestralFear Cards.ancestralFear

instance RunMessage AncestralFear where
  runMessage msg t@(AncestralFear attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      mLocation <- selectOne $ locationWithInvestigator iid
      chooseOrRunOneM iid $ campaignI18n do
        for_ mLocation \lid -> labeled' "ancestralFear.placeDoom" $ placeDoom attrs lid 1
        labeled' "ancestralFear.addToVictory" $ addToVictory attrs
      pure t
    _ -> AncestralFear <$> liftRunMessage msg attrs
