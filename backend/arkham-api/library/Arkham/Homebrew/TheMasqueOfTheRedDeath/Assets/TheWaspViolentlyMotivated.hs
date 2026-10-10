module Arkham.Homebrew.TheMasqueOfTheRedDeath.Assets.TheWaspViolentlyMotivated (
  theWaspViolentlyMotivated,
) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted hiding (InvestigatorDamage)
import Arkham.GameEnv (getSkillTest)
import Arkham.Helpers.Modifiers (ModifierType (Difficulty), maybeModified_)
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.TheMasqueOfTheRedDeath.Helpers (guestParleySuccess)
import Arkham.Investigator.Types (Field (InvestigatorDamage))
import Arkham.Projection

newtype TheWaspViolentlyMotivated = TheWaspViolentlyMotivated AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theWaspViolentlyMotivated :: AssetCard TheWaspViolentlyMotivated
theWaspViolentlyMotivated = asset TheWaspViolentlyMotivated Cards.theWaspViolentlyMotivated

instance HasAbilities TheWaspViolentlyMotivated where
  -- "[action]: Parley. Test [combat] (4)."
  getAbilities (TheWaspViolentlyMotivated a) =
    [skillTestAbility $ restricted a 1 OnSameLocation parleyAction_]

instance HasModifiersFor TheWaspViolentlyMotivated where
  -- "This test gets -1 difficulty for every 2 damage on your investigator (to a
  -- minimum of 1)." The printed difficulty is 4, so the reduction stops at 3.
  getModifiersFor (TheWaspViolentlyMotivated a) =
    getSkillTest >>= traverse_ \st -> maybeModified_ a (SkillTestTarget st.id) do
      guard $ isAbilitySource a 1 st.source
      damage <- lift $ field InvestigatorDamage st.investigator
      let n = min 3 (damage `div` 2)
      guard $ n > 0
      pure [Difficulty (-n)]

instance RunMessage TheWaspViolentlyMotivated where
  runMessage msg a@(TheWaspViolentlyMotivated attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) attrs #combat (Fixed 4)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      guestParleySuccess attrs 1 iid
      pure a
    -- Act 1b flips every guest; the Victim face is a different card def.
    Flip _ _ (isTarget attrs -> True) -> do
      push $ ReplaceAsset attrs.id Cards.theWaspViolentlyEmbraced
      pure a
    _ -> TheWaspViolentlyMotivated <$> liftRunMessage msg attrs
