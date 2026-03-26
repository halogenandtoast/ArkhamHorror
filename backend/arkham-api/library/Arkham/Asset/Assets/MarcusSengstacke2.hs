module Arkham.Asset.Assets.MarcusSengstacke2 (marcusSengstacke2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Matcher

newtype MarcusSengstacke2 = MarcusSengstacke2 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

marcusSengstacke2 :: AssetCard MarcusSengstacke2
marcusSengstacke2 = ally MarcusSengstacke2 Cards.marcusSengstacke2 (1, 3)

instance HasModifiersFor MarcusSengstacke2 where
  getModifiersFor (MarcusSengstacke2 a) = controllerGets a [UpkeepResources 1]

instance HasAbilities MarcusSengstacke2 where
  getAbilities (MarcusSengstacke2 a) =
    [controlled_ a 1 $ forced $ SkillTestResult #after You AnySkillTest #failure]

instance RunMessage MarcusSengstacke2 where
  runMessage msg a@(MarcusSengstacke2 attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      dealAssetHorror attrs.id (attrs.ability 1) 1
      pure a
    _ -> MarcusSengstacke2 <$> liftRunMessage msg attrs
