module Arkham.Asset.Assets.ProfaneIdol (profaneIdol) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (AssetDefeated)
import Arkham.Matcher

newtype ProfaneIdol = ProfaneIdol AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

profaneIdol :: AssetCard ProfaneIdol
profaneIdol = assetWith ProfaneIdol Cards.profaneIdol (sanityL ?~ 3)

instance HasAbilities ProfaneIdol where
  getAbilities (ProfaneIdol a) = [restrictedAbility a 1 ControlsThis $ forced $ AssetDefeated #when ByAny (be a)]

instance RunMessage ProfaneIdol where
  runMessage msg a@(ProfaneIdol attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      shuffleIntoDeck iid attrs
      pure a
    _ -> ProfaneIdol <$> liftRunMessage msg attrs
