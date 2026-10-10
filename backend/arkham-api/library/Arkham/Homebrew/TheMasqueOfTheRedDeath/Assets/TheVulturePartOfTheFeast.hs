module Arkham.Homebrew.TheMasqueOfTheRedDeath.Assets.TheVulturePartOfTheFeast (
  theVulturePartOfTheFeast,
) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Cost (getCanAffordCost, payEffectCost)
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.TheMasqueOfTheRedDeath.Helpers (guestEntryToll, guestParleySuccess)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier (ModifierType (SkillTestAutomaticallySucceeds))

newtype TheVulturePartOfTheFeast = TheVulturePartOfTheFeast AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theVulturePartOfTheFeast :: AssetCard TheVulturePartOfTheFeast
theVulturePartOfTheFeast =
  assetWith TheVulturePartOfTheFeast Cards.theVulturePartOfTheFeast ((healthL ?~ 1) . (sanityL ?~ 1))

instance HasAbilities TheVulturePartOfTheFeast where
  -- "Forced - After you enter The Vulture's location: ..." and
  -- "[action]: Parley. Test [intellect] (4)."
  getAbilities (TheVulturePartOfTheFeast a) =
    [ mkAbility a 1 $ forced $ Enters #after You (locationWithAsset a.id)
    , skillTestAbility $ restricted a 2 OnSameLocation parleyAction_
    ]

instance RunMessage TheVulturePartOfTheFeast where
  runMessage msg a@(TheVulturePartOfTheFeast attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      guestEntryToll attrs iid
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      -- "When you initiate this skill test, you may spend 3 resources to make
      -- this test automatically succeed."
      let cost = ResourceCost 3
      whenM (getCanAffordCost iid (attrs.ability 2) [] [] cost) do
        chooseOneM iid $ withI18n do
          countVar 3 $ labeled "spendResources" do
            payEffectCost iid attrs cost
            skillTestModifier sid (attrs.ability 2) sid SkillTestAutomaticallySucceeds
          skip_
      parley sid iid (attrs.ability 2) attrs #intellect (Fixed 4)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      guestParleySuccess attrs 2 iid
      pure a
    _ -> TheVulturePartOfTheFeast <$> liftRunMessage msg attrs
