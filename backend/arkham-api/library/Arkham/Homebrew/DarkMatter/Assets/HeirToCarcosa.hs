module Arkham.Homebrew.DarkMatter.Assets.HeirToCarcosa (heirToCarcosa) where

import Arkham.Asset.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Message (ShuffleIn (..))
import Arkham.Message.Lifted.Placement

newtype HeirToCarcosa = HeirToCarcosa AssetAttrs
  deriving anyclass (IsAsset, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

heirToCarcosa :: AssetCard HeirToCarcosa
heirToCarcosa = asset HeirToCarcosa Cards.heirToCarcosa

instance RunMessage HeirToCarcosa where
  runMessage msg a@(HeirToCarcosa attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      addCampaignCardToDeck iid DoNotShuffleIn Cards.heirToCarcosa
      place attrs (InPlayArea iid)
      pure a
    _ -> HeirToCarcosa <$> liftRunMessage msg attrs
