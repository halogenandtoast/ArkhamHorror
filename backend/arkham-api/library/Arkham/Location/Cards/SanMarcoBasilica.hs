module Arkham.Location.Cards.SanMarcoBasilica (sanMarcoBasilica) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Classes
import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Message.Lifted.Choose

newtype SanMarcoBasilica = SanMarcoBasilica LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sanMarcoBasilica :: LocationCard SanMarcoBasilica
sanMarcoBasilica =
  locationWith
    SanMarcoBasilica
    Cards.sanMarcoBasilica
    3
    (Static 0)
    (connectsToL .~ singleton RightOf)

instance HasAbilities SanMarcoBasilica where
  getAbilities (SanMarcoBasilica a) =
    extendRevealed1 a
      $ restricted
        a
        1
        (Here <> exists (AssetControlledBy You <> assetIs Assets.innocentReveler))
        actionAbility

instance RunMessage SanMarcoBasilica where
  runMessage msg l@(SanMarcoBasilica attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      innocentRevelers <- select $ AssetControlledBy You <> assetIs Assets.innocentReveler
      chooseOneM iid do
        targets innocentRevelers \innocentReveler -> do
          card <- fetchCard innocentReveler
          placeUnderneath ActDeckTarget [card]
      pure l
    _ -> SanMarcoBasilica <$> liftRunMessage msg attrs
