module Arkham.Treachery.Cards.SellYourSoul (sellYourSoul) where

import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Treachery.CardDefs.ReturnTo qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SellYourSoul = SellYourSoul TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sellYourSoul :: TreacheryCard SellYourSoul
sellYourSoul = treachery SellYourSoul Cards.sellYourSoul

instance RunMessage SellYourSoul where
  runMessage msg t@(SellYourSoul attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      hasResources <- fieldMap InvestigatorResources (>= 10) iid
      if hasResources
        then loseResources iid attrs 10
        else drivenInsane iid
      pure t
    _ -> SellYourSoul <$> liftRunMessage msg attrs
