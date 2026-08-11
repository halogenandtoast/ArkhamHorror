module Arkham.Homebrew.DarkMatter.Assets.RadiationTablets (radiationTablets) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (campaignI18n, shuffleIntoScanningDeck)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype RadiationTablets = RadiationTablets AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

radiationTablets :: AssetCard RadiationTablets
radiationTablets = asset RadiationTablets Cards.radiationTablets

instance HasAbilities RadiationTablets where
  getAbilities (RadiationTablets a) =
    [controlled_ a 1 $ FastAbility (assetUseCost a Supply 1)]

instance RunMessage RadiationTablets where
  runMessage msg a@(RadiationTablets attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      investigators <- select Anyone
      chooseOneM iid $ campaignI18n do
        targets investigators \iid' -> putCardIntoPlay iid' attrs
        labeled' "radiationTablets.doNotPutIntoPlay" $ shuffleIntoScanningDeck [attrs]
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      roundModifiers (attrs.ability 1) iid [SkillModifier #combat 1, SkillModifier #agility 1]
      pure a
    _ -> RadiationTablets <$> liftRunMessage msg attrs
