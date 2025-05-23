module Arkham.Asset.Assets.ArtStudent (artStudent) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher

newtype ArtStudent = ArtStudent AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

artStudent :: AssetCard ArtStudent
artStudent = ally ArtStudent Cards.artStudent (1, 2)

instance HasAbilities ArtStudent where
  getAbilities (ArtStudent x) =
    [ controlled x 1 (exists (You <> InvestigatorCanDiscoverCluesAt YourLocation))
        $ freeReaction (AssetEntersPlay #when (be x))
    ]

instance RunMessage ArtStudent where
  runMessage msg a@(ArtStudent attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discoverAtYourLocation NotInvestigate iid (attrs.ability 1) 1
      pure a
    _ -> ArtStudent <$> liftRunMessage msg attrs
