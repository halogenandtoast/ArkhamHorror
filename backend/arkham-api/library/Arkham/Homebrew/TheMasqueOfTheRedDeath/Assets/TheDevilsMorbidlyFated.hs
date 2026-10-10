module Arkham.Homebrew.TheMasqueOfTheRedDeath.Assets.TheDevilsMorbidlyFated (
  theDevilsMorbidlyFated,
) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.TheMasqueOfTheRedDeath.Helpers (guestEntryToll, guestParleySuccess)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier (ModifierType (SkillTestAutomaticallySucceeds))

newtype TheDevilsMorbidlyFated = TheDevilsMorbidlyFated AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theDevilsMorbidlyFated :: AssetCard TheDevilsMorbidlyFated
theDevilsMorbidlyFated =
  assetWith TheDevilsMorbidlyFated Cards.theDevilsMorbidlyFated ((healthL ?~ 2) . (sanityL ?~ 2))

instance HasAbilities TheDevilsMorbidlyFated where
  -- "Forced - After you enter The Devils' location: ..." and
  -- "[action]: Parley. Test [willpower] (4)."
  getAbilities (TheDevilsMorbidlyFated a) =
    [ mkAbility a 1 $ forced $ Enters #after You (locationWithAsset a.id)
    , skillTestAbility $ restricted a 2 OnSameLocation parleyAction_
    ]

instance RunMessage TheDevilsMorbidlyFated where
  runMessage msg a@(TheDevilsMorbidlyFated attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      guestEntryToll attrs iid
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      -- "When you initiate this skill test, you may take 1 direct horror to make
      -- this test automatically succeed."
      canTakeHorror <- iid <=~> InvestigatorCanBeDamaged
      chooseOneM iid $ withI18n do
        when canTakeHorror $ countVar 1 $ labeled "takeDirectHorror" do
          directDamageAndHorror iid (attrs.ability 2) 0 1
          skillTestModifier sid (attrs.ability 2) sid SkillTestAutomaticallySucceeds
        labeled "skip" nothing
      parley sid iid (attrs.ability 2) attrs #willpower (Fixed 4)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      guestParleySuccess attrs 2 iid
      pure a
    _ -> TheDevilsMorbidlyFated <$> liftRunMessage msg attrs
