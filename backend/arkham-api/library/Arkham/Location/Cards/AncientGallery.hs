module Arkham.Location.Cards.AncientGallery (ancientGallery) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers (increaseThisFloodLevel)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype AncientGallery = AncientGallery LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientGallery :: LocationCard AncientGallery
ancientGallery = location AncientGallery Cards.ancientGallery 3 (Static 3)

instance HasAbilities AncientGallery where
  getAbilities (AncientGallery a) =
    if a.revealed
      then
        extendRevealed1 a
          $ groupLimit PerGame
          $ restricted a 1 Here
          $ actionAbilityWithCost (HandDiscardCost 2 #any)
      else extendUnrevealed1 a $ mkAbility a 2 $ forced $ Enters #when You (be a)

instance RunMessage AncientGallery where
  runMessage msg l@(AncientGallery attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      takeControlOfSetAsideAsset iid =<< getSetAsideCard Assets.obsidianRelic
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      increaseThisFloodLevel attrs
      floodable <- select $ connectedTo (be attrs) <> CanHaveFloodLevelIncreased
      chooseTargetM iid floodable increaseThisFloodLevel
      pure l
    _ -> AncientGallery <$> liftRunMessage msg attrs
