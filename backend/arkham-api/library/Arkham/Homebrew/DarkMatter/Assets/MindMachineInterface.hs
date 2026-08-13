module Arkham.Homebrew.DarkMatter.Assets.MindMachineInterface (mindMachineInterface) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (campaignI18n, scan, shuffleIntoScanningDeck)
import Arkham.LocationSymbol qualified as LS
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype MindMachineInterface = MindMachineInterface AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mindMachineInterface :: AssetCard MindMachineInterface
mindMachineInterface = asset MindMachineInterface Cards.mindMachineInterface

instance HasAbilities MindMachineInterface where
  getAbilities (MindMachineInterface a) =
    [ controlled_ a 1 $ actionAbilityWithCost (GroupClueCost (PerPlayer 2) Anywhere)
    ]

instance RunMessage MindMachineInterface where
  runMessage msg a@(MindMachineInterface attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      investigators <- select Anyone
      chooseOneM iid $ campaignI18n do
        targets investigators (`putCardIntoPlay` attrs)
        labeled' "mindMachineInterface.doNotPutIntoPlay" $ shuffleIntoScanningDeck [attrs]
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      scan iid (attrs.ability 1) [LS.Trefoil]
      pure a
    _ -> MindMachineInterface <$> liftRunMessage msg attrs
