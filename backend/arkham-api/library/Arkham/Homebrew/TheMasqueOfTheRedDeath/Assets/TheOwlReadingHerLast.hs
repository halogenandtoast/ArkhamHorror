module Arkham.Homebrew.TheMasqueOfTheRedDeath.Assets.TheOwlReadingHerLast (
  theOwlReadingHerLast,
) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Message.Discard.Lifted (chooseAndDiscardCards)
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.TheMasqueOfTheRedDeath.Helpers (guestEntryToll, guestParleySuccess)
import Arkham.I18n
import Arkham.Investigator.Types (Field (InvestigatorHand))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier (ModifierType (SkillTestAutomaticallySucceeds))
import Arkham.Projection

newtype TheOwlReadingHerLast = TheOwlReadingHerLast AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theOwlReadingHerLast :: AssetCard TheOwlReadingHerLast
theOwlReadingHerLast =
  assetWith TheOwlReadingHerLast Cards.theOwlReadingHerLast ((healthL ?~ 1) . (sanityL ?~ 1))

instance HasAbilities TheOwlReadingHerLast where
  -- "Forced - After you enter The Owl's location: ..." and
  -- "[action]: Parley. Test [intellect] (4)."
  getAbilities (TheOwlReadingHerLast a) =
    [ mkAbility a 1 $ forced $ Enters #after You (locationWithAsset a.id)
    , skillTestAbility $ restricted a 2 OnSameLocation parleyAction_
    ]

instance RunMessage TheOwlReadingHerLast where
  runMessage msg a@(TheOwlReadingHerLast attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      guestEntryToll attrs iid
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      -- "When you initiate this skill test, you may discard 2 cards from your hand
      -- to make this test automatically succeed."
      hand <- fieldMap InvestigatorHand length iid
      chooseOneM iid $ withI18n do
        when (hand >= 2) $ countVar 2 $ labeled "discardCardsFromHand" do
          chooseAndDiscardCards iid (attrs.ability 2) 2
          skillTestModifier sid (attrs.ability 2) sid SkillTestAutomaticallySucceeds
        labeled "skip" nothing
      parley sid iid (attrs.ability 2) attrs #intellect (Fixed 4)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      guestParleySuccess attrs 2 iid
      pure a
    _ -> TheOwlReadingHerLast <$> liftRunMessage msg attrs
