module Arkham.Homebrew.TheMasqueOfTheRedDeath.Assets.TheWaspViolentlyEmbraced (
  theWaspViolentlyEmbraced,
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

newtype TheWaspViolentlyEmbraced = TheWaspViolentlyEmbraced AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theWaspViolentlyEmbraced :: AssetCard TheWaspViolentlyEmbraced
theWaspViolentlyEmbraced =
  assetWith TheWaspViolentlyEmbraced Cards.theWaspViolentlyEmbraced ((healthL ?~ 1) . (sanityL ?~ 1))

instance HasAbilities TheWaspViolentlyEmbraced where
  -- "Forced - After you enter The Wasp's location: ..." and
  -- "[action]: Parley. Test [combat] (4)."
  getAbilities (TheWaspViolentlyEmbraced a) =
    [ mkAbility a 1 $ forced $ Enters #after You (locationWithAsset a.id)
    , skillTestAbility $ restricted a 2 OnSameLocation parleyAction_
    ]

instance RunMessage TheWaspViolentlyEmbraced where
  runMessage msg a@(TheWaspViolentlyEmbraced attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      guestEntryToll attrs iid
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      -- "When you initiate this skill test, you may take 1 direct damage to make
      -- this test automatically succeed."
      let cost = DirectDamageCost (attrs.ability 2) (InvestigatorWithId iid) 1
      whenM (getCanAffordCost iid (attrs.ability 2) [] [] cost) do
        chooseOneM iid $ withI18n do
          countVar 1 $ labeled "takeDirectDamage" do
            payEffectCost iid attrs cost
            skillTestModifier sid (attrs.ability 2) sid SkillTestAutomaticallySucceeds
          skip_
      parley sid iid (attrs.ability 2) attrs #combat (Fixed 4)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      guestParleySuccess attrs 2 iid
      pure a
    _ -> TheWaspViolentlyEmbraced <$> liftRunMessage msg attrs
