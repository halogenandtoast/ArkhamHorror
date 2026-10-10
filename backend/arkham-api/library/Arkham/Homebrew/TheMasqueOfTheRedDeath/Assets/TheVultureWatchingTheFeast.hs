module Arkham.Homebrew.TheMasqueOfTheRedDeath.Assets.TheVultureWatchingTheFeast (
  theVultureWatchingTheFeast,
) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.GameEnv (getSkillTest)
import Arkham.Helpers.Modifiers (ModifierType (Difficulty), maybeModified_)
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.TheMasqueOfTheRedDeath.Helpers (guestParleySuccess)
import Arkham.Investigator.Types (Field (InvestigatorResources))
import Arkham.Projection

newtype TheVultureWatchingTheFeast = TheVultureWatchingTheFeast AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theVultureWatchingTheFeast :: AssetCard TheVultureWatchingTheFeast
theVultureWatchingTheFeast = asset TheVultureWatchingTheFeast Cards.theVultureWatchingTheFeast

instance HasAbilities TheVultureWatchingTheFeast where
  -- "[action]: Parley. Test [intellect] (4)."
  getAbilities (TheVultureWatchingTheFeast a) =
    [skillTestAbility $ restricted a 1 OnSameLocation parleyAction_]

instance HasModifiersFor TheVultureWatchingTheFeast where
  -- "This test gets -1 difficulty for every 4 resources in your resource pool (to
  -- a minimum of 1)." The printed difficulty is 4, so the reduction stops at 3.
  getModifiersFor (TheVultureWatchingTheFeast a) =
    getSkillTest >>= traverse_ \st -> maybeModified_ a (SkillTestTarget st.id) do
      guard $ isAbilitySource a 1 st.source
      resources <- lift $ field InvestigatorResources st.investigator
      let n = min 3 (resources `div` 4)
      guard $ n > 0
      pure [Difficulty (-n)]

instance RunMessage TheVultureWatchingTheFeast where
  runMessage msg a@(TheVultureWatchingTheFeast attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) attrs #intellect (Fixed 4)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      guestParleySuccess attrs 1 iid
      pure a
    -- Act 1b flips every guest; the Victim face is a different card def.
    Flip _ _ (isTarget attrs -> True) -> do
      push $ ReplaceAsset attrs.id Cards.theVulturePartOfTheFeast
      pure a
    _ -> TheVultureWatchingTheFeast <$> liftRunMessage msg attrs
