module Arkham.Homebrew.TheMasqueOfTheRedDeath.Assets.TheFoxLookingForRelease (
  theFoxLookingForRelease,
) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Card (cardMatch, card_)
import Arkham.Helpers.Message.Discard.Lifted (chooseAndDiscardCardEdit, discardFilter)
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.TheMasqueOfTheRedDeath.Helpers (
  guestEntryToll,
  guestParleySuccess,
  scenarioI18n,
 )
import Arkham.I18n
import Arkham.Investigator.Types (Field (InvestigatorHand))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier (ModifierType (SkillTestAutomaticallySucceeds))
import Arkham.Projection

newtype TheFoxLookingForRelease = TheFoxLookingForRelease AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theFoxLookingForRelease :: AssetCard TheFoxLookingForRelease
theFoxLookingForRelease =
  assetWith TheFoxLookingForRelease Cards.theFoxLookingForRelease ((healthL ?~ 1) . (sanityL ?~ 1))

instance HasAbilities TheFoxLookingForRelease where
  -- "Forced - After you enter The Fox's location: ..." and
  -- "[action]: Parley. Test [agility] (4)."
  getAbilities (TheFoxLookingForRelease a) =
    [ mkAbility a 1 $ forced $ Enters #after You (locationWithAsset a.id)
    , skillTestAbility $ restricted a 2 OnSameLocation parleyAction_
    ]

instance RunMessage TheFoxLookingForRelease where
  runMessage msg a@(TheFoxLookingForRelease attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      guestEntryToll attrs iid
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      -- "When you initiate this skill test, you may discard an event from your
      -- hand to make this test automatically succeed."
      hasEvent <- fieldMap InvestigatorHand (any (`cardMatch` card_ #event)) iid
      chooseOneM iid $ scenarioI18n $ scope "theFoxLookingForRelease" do
        when hasEvent $ labeled "discardEvent" do
          chooseAndDiscardCardEdit iid (attrs.ability 2) \d -> d {discardFilter = card_ #event}
          skillTestModifier sid (attrs.ability 2) sid SkillTestAutomaticallySucceeds
        unscoped skip_
      parley sid iid (attrs.ability 2) attrs #agility (Fixed 4)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      guestParleySuccess attrs 2 iid
      pure a
    _ -> TheFoxLookingForRelease <$> liftRunMessage msg attrs
