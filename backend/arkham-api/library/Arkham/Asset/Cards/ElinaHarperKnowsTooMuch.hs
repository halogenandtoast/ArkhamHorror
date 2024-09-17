module Arkham.Asset.Cards.ElinaHarperKnowsTooMuch
  ( elinaHarperKnowsTooMuch
  , ElinaHarperKnowsTooMuch(..)
  )
where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype ElinaHarperKnowsTooMuch = ElinaHarperKnowsTooMuch AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

elinaHarperKnowsTooMuch :: AssetCard ElinaHarperKnowsTooMuch
elinaHarperKnowsTooMuch = asset ElinaHarperKnowsTooMuch Cards.elinaHarperKnowsTooMuch

instance RunMessage ElinaHarperKnowsTooMuch where
  runMessage msg (ElinaHarperKnowsTooMuch attrs) = runQueueT $ case msg of
    _ -> ElinaHarperKnowsTooMuch <$> liftRunMessage msg attrs
