module Arkham.Treachery.Cards.BurdenOfLeadership (burdenOfLeadership) where

import Arkham.Classes.HasQueue
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype BurdenOfLeadership = BurdenOfLeadership TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

burdenOfLeadership :: TreacheryCard BurdenOfLeadership
burdenOfLeadership = treachery BurdenOfLeadership Cards.burdenOfLeadership

instance RunMessage BurdenOfLeadership where
  runMessage msg t@(BurdenOfLeadership attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      allies <-
        select $ assetControlledBy iid <> #ally <> oneOf [AssetReady, AssetWithHealth, AssetWithSanity]

      if null allies
        then shuffleIntoDeck iid attrs
        else do
          chooseOneAtATimeM iid do
            targets allies \ally -> do
              chooseOneM iid do
                whenMatch ally AssetReady $ labeled "Exhaust" $ exhaustThis ally
                whenMatch ally (oneOf [AssetWithHealth, AssetWithSanity]) do
                  labeled "Deal 1 direct damage and 1 direct horror"
                    $ dealAssetDirectDamageAndHorror ally attrs 1 1
      pure t
    _ -> BurdenOfLeadership <$> liftRunMessage msg attrs
