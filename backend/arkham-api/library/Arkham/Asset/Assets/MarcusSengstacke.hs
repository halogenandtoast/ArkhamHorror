module Arkham.Asset.Assets.MarcusSengstacke (marcusSengstacke) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Matcher

newtype MarcusSengstacke = MarcusSengstacke AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

marcusSengstacke :: AssetCard MarcusSengstacke
marcusSengstacke = ally MarcusSengstacke Cards.marcusSengstacke (1, 2)

instance HasModifiersFor MarcusSengstacke where
  getModifiersFor (MarcusSengstacke a) = controllerGets a [UpkeepResources 1]

instance HasAbilities MarcusSengstacke where
  getAbilities (MarcusSengstacke a) =
    [controlled_ a 1 $ forced $ SkillTestResult #after You AnySkillTest #failure]

instance RunMessage MarcusSengstacke where
  runMessage msg a@(MarcusSengstacke attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      dealAssetHorror attrs.id (attrs.ability 1) 1
      pure a
    _ -> MarcusSengstacke <$> liftRunMessage msg attrs
