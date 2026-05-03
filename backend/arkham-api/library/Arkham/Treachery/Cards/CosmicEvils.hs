module Arkham.Treachery.Cards.CosmicEvils (cosmicEvils) where

import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CosmicEvils = CosmicEvils TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cosmicEvils :: TreacheryCard CosmicEvils
cosmicEvils = treachery CosmicEvils Cards.cosmicEvils

instance RunMessage CosmicEvils where
  runMessage msg t@(CosmicEvils attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      chooseOneM iid $ withI18n do
        countVar 1 $ labeled' "placeAgendaDoom" do
          placeDoomOnAgendaAndCheckAdvance 1
        labeled' "core2.cosmicEvils.option" do
          directDamageAndHorror iid attrs 1 1
          gainSurge attrs
      pure t
    _ -> CosmicEvils <$> liftRunMessage msg attrs
