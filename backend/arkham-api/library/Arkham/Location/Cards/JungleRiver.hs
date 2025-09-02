module Arkham.Location.Cards.JungleRiver (jungleRiver) where

import Arkham.Ability
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.FilmFatale.Helpers
import Arkham.Trait (Trait (Clothing, Weapon))

newtype JungleRiver = JungleRiver LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jungleRiver :: LocationCard JungleRiver
jungleRiver = location JungleRiver Cards.jungleRiver 0 (Static 0)

instance HasAbilities JungleRiver where
  getAbilities (JungleRiver a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ Leaves #after You (be a)

instance RunMessage JungleRiver where
  runMessage msg l@(JungleRiver attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 2)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assets <- select (assetControlledBy iid <> hasAnyTrait [Weapon, Clothing] <> DiscardableAsset)
      chooseOneM iid do
        withI18n $ countVar 1 $ labeled' "takeDirectDamage" $ directDamage iid (attrs.ability 1) 1
        scenarioI18n $ labeledValidate' (notNull assets) "jungleRiver.option" do
          chooseTargetM iid assets (toDiscardBy iid (attrs.ability 1))
      pure l
    _ -> JungleRiver <$> liftRunMessage msg attrs
