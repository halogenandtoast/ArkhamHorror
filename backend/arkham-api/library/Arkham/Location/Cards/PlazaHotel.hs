module Arkham.Location.Cards.PlazaHotel (plazaHotel) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Concealed
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype PlazaHotel = PlazaHotel LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

plazaHotel :: LocationCard PlazaHotel
plazaHotel = symbolLabel $ location PlazaHotel Cards.plazaHotel 2 (PerPlayer 1)

instance HasAbilities PlazaHotel where
  getAbilities (PlazaHotel a) =
    extendRevealed1 a $ restricted a 1 Here $ FastAbility (HorrorCost (toSource a) YouTarget 1)

instance RunMessage PlazaHotel where
  runMessage msg l@(PlazaHotel attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- select (orConnected_ (LocationWithId attrs.id) <> LocationWithConcealedCard)
      chooseTargetM iid locations \lid -> do
        concealed <- map toId <$> getConcealedAt (ForExpose $ toSource attrs) lid
        chooseOrRunOneM iid $ targets concealed $ revealConcealed iid (attrs.ability 1)
      pure l
    _ -> PlazaHotel <$> liftRunMessage msg attrs
