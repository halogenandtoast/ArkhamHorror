module Arkham.Treachery.Cards.BoughtInBlood (boughtInBlood) where

import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype BoughtInBlood = BoughtInBlood TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

boughtInBlood :: TreacheryCard BoughtInBlood
boughtInBlood = treachery BoughtInBlood Cards.boughtInBlood

instance RunMessage BoughtInBlood where
  runMessage msg t@(BoughtInBlood attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      inPlay <- select $ assetControlledBy iid <> #ally
      inHand <- select $ inHandOf NotForPlay iid <> basic #ally
      if null inPlay && null inHand
        then shuffleIntoDeck iid attrs
        else chooseOrRunOneM iid do
          unless (null inPlay) do
            labeled "Discard an Ally asset you control from play" do
              chooseAndDiscardAssetMatching iid attrs #ally

          unless (null inHand) do
            labeled "Discard each Ally asset from your hand" do
              for_ inHand (discardCard iid attrs)
      pure t
    _ -> BoughtInBlood <$> liftRunMessage msg attrs
