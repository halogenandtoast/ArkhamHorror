module Arkham.Homebrew.TheFallOfTheHouseOfUsher.Assets.MadelineUsher (madelineUsher) where

import Arkham.Asset.Import.Lifted
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Assets qualified as Cards
import Arkham.I18n
import Arkham.Message.Lifted.Choose

newtype MadelineUsher = MadelineUsher AssetAttrs
  deriving anyclass (IsAsset, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

madelineUsher :: AssetCard MadelineUsher
madelineUsher = asset MadelineUsher Cards.madelineUsher

instance RunMessage MadelineUsher where
  runMessage msg a@(MadelineUsher attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      eachInvestigator \iid ->
        chooseOneM iid $ withI18n do
          labeled "drawCardOne" $ drawCards iid attrs 1
          labeled "doNotDraw" nothing
      toDiscard attrs attrs
      pure a
    _ -> MadelineUsher <$> liftRunMessage msg attrs
