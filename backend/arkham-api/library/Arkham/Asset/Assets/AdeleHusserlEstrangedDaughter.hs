module Arkham.Asset.Assets.AdeleHusserlEstrangedDaughter (adeleHusserlEstrangedDaughter) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Scenarios.TracesToNowhere.Helpers
import Arkham.Trait (Trait (Arkham))

newtype AdeleHusserlEstrangedDaughter = AdeleHusserlEstrangedDaughter AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

adeleHusserlEstrangedDaughter :: AssetCard AdeleHusserlEstrangedDaughter
adeleHusserlEstrangedDaughter =
  allyWith AdeleHusserlEstrangedDaughter Cards.adeleHusserlEstrangedDaughter (2, 2) noSlots

instance HasAbilities AdeleHusserlEstrangedDaughter where
  getAbilities (AdeleHusserlEstrangedDaughter a) =
    [skillTestAbility $ restricted a 1 OnSameLocation parleyAction_]

instance RunMessage AdeleHusserlEstrangedDaughter where
  runMessage msg a@(AdeleHusserlEstrangedDaughter attrs) = runQueueT $ scenarioI18n $ case msg of
    -- "Put Adele Husserl into play at the Arkham location farthest from each investigator."
    Revelation iid (isSource attrs -> True) -> do
      locations <- select $ FarthestLocationFromAll $ LocationWithTrait Arkham
      chooseOrRunOneM iid $ targets locations $ place attrs . AtLocation
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      witnessParley sid iid (attrs.ability 1) #intellect
      pure a
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      codexEntry "codex.adeleHusserl"
      sealSymbolAndClaim attrs #tablet
      pure a
    _ -> AdeleHusserlEstrangedDaughter <$> liftRunMessage msg attrs
