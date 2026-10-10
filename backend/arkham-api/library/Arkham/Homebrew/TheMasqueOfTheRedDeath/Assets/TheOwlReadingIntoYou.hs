module Arkham.Homebrew.TheMasqueOfTheRedDeath.Assets.TheOwlReadingIntoYou (
  theOwlReadingIntoYou,
) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.GameEnv (getSkillTest)
import Arkham.Helpers.Modifiers (ModifierType (Difficulty), maybeModified_)
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.TheMasqueOfTheRedDeath.Helpers (guestParleySuccess)
import Arkham.Investigator.Types (Field (InvestigatorHand))
import Arkham.Projection

newtype TheOwlReadingIntoYou = TheOwlReadingIntoYou AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theOwlReadingIntoYou :: AssetCard TheOwlReadingIntoYou
theOwlReadingIntoYou = asset TheOwlReadingIntoYou Cards.theOwlReadingIntoYou

instance HasAbilities TheOwlReadingIntoYou where
  -- "[action]: Parley. Test [intellect] (4)."
  getAbilities (TheOwlReadingIntoYou a) =
    [skillTestAbility $ restricted a 1 OnSameLocation parleyAction_]

instance HasModifiersFor TheOwlReadingIntoYou where
  -- "This test gets -1 difficulty for every 3 cards in your hand (to a minimum of
  -- 1)." The printed difficulty is 4, so the reduction stops at 3.
  getModifiersFor (TheOwlReadingIntoYou a) =
    getSkillTest >>= traverse_ \st -> maybeModified_ a (SkillTestTarget st.id) do
      guard $ isAbilitySource a 1 st.source
      hand <- lift $ fieldMap InvestigatorHand length st.investigator
      let n = min 3 (hand `div` 3)
      guard $ n > 0
      pure [Difficulty (-n)]

instance RunMessage TheOwlReadingIntoYou where
  runMessage msg a@(TheOwlReadingIntoYou attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) attrs #intellect (Fixed 4)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      guestParleySuccess attrs 1 iid
      pure a
    -- Act 1b flips every guest; the Victim face is a different card def.
    Flip _ _ (isTarget attrs -> True) -> do
      push $ ReplaceAsset attrs.id Cards.theOwlReadingHerLast
      pure a
    _ -> TheOwlReadingIntoYou <$> liftRunMessage msg attrs
