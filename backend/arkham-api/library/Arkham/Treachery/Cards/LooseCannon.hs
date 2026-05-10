module Arkham.Treachery.Cards.LooseCannon (looseCannon) where

import Arkham.I18n
import Arkham.Investigator.Projection ()
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype LooseCannon = LooseCannon TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

looseCannon :: TreacheryCard LooseCannon
looseCannon = treachery LooseCannon Cards.looseCannon

instance RunMessage LooseCannon where
  runMessage msg t@(LooseCannon attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      firearms <- select $ assetControlledBy iid <> #firearm
      resources <- iid.resources
      chooseOrRunOneM iid do
        when (notNull firearms) do
          labeledI "discardEachFirearmAsset" do
            chooseOneAtATimeM iid $ targets firearms $ toDiscardBy iid attrs
        when (resources >= 1) do
          withI18n $ countVar 5 $ labeled' "loseResources" $ loseResources iid attrs 5
      pure t
    _ -> LooseCannon <$> liftRunMessage msg attrs
