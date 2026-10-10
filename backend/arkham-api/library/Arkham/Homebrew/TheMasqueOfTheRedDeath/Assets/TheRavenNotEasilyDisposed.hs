module Arkham.Homebrew.TheMasqueOfTheRedDeath.Assets.TheRavenNotEasilyDisposed (
  theRavenNotEasilyDisposed,
) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Cost (getCanAffordCost, payEffectCost)
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.TheMasqueOfTheRedDeath.Helpers (
  guestEntryToll,
  guestParleySuccess,
  scenarioI18n,
 )
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier (ModifierType (SkillTestAutomaticallySucceeds))

newtype TheRavenNotEasilyDisposed = TheRavenNotEasilyDisposed AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theRavenNotEasilyDisposed :: AssetCard TheRavenNotEasilyDisposed
theRavenNotEasilyDisposed =
  assetWith
    TheRavenNotEasilyDisposed
    Cards.theRavenNotEasilyDisposed
    ((healthL ?~ 1) . (sanityL ?~ 1))

instance HasAbilities TheRavenNotEasilyDisposed where
  -- "Forced - After you enter The Raven's location: ..." and
  -- "[action]: Parley. Test [agility] (4)."
  getAbilities (TheRavenNotEasilyDisposed a) =
    [ mkAbility a 1 $ forced $ Enters #after You (locationWithAsset a.id)
    , skillTestAbility $ restricted a 2 OnSameLocation parleyAction_
    ]

instance RunMessage TheRavenNotEasilyDisposed where
  runMessage msg a@(TheRavenNotEasilyDisposed attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      guestEntryToll attrs iid
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      -- "When you initiate this skill test, you may discard a skill from your
      -- hand to make this test automatically succeed."
      let cost = HandDiscardCost 1 #skill
      whenM (getCanAffordCost iid (attrs.ability 2) [] [] cost) do
        chooseOneM iid $ scenarioI18n $ scope "theRavenNotEasilyDisposed" do
          labeled "discardSkill" do
            payEffectCost iid attrs cost
            skillTestModifier sid (attrs.ability 2) sid SkillTestAutomaticallySucceeds
          unscoped skip_
      parley sid iid (attrs.ability 2) attrs #agility (Fixed 4)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      guestParleySuccess attrs 2 iid
      pure a
    _ -> TheRavenNotEasilyDisposed <$> liftRunMessage msg attrs
